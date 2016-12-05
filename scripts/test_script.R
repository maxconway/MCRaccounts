michaelmas_charged <- read_tsv('~/Downloads/michaelmas_bill.tsv') %>%
	select(crsid, owes)
easter_charged <- read_tsv('~/Downloads/Easter.tsv') %>%
	select(crsid = CRSID, amount)

michaelmas_online <- read_tsv('~/Downloads/online_michaelmas_2015.tsv') %>%
	select(crsid = suid, owes = Owes)
easter_online <- read_tsv('~/Downloads/online_easter.tsv') %>%
	select(crsid = suid, owes = Owes)

# Easter check
charged_tickets %>%
	filter(time %within% new_interval(dmy('24/3/2015'), dmy('3/6/2015'))) %>%
	group_by(crsid) %>%
	summarise(new_value = sum(price, na.rm=TRUE)) %>%
	full_join(deductions %>%
							filter(time %within% new_interval(dmy('24/3/2015'), dmy('3/6/2015'))) %>%
							group_by(crsid) %>%
							summarise(deduction = sum(price, na.rm=TRUE))) %>%
	full_join(easter_charged) %>%
	rename(charged_value = amount) %>%
	full_join(easter_online) %>%
	rename(online_value = owes) %>%
	filter((pmax(new_value,charged_value,online_value-deduction, na.rm = TRUE) - pmin(new_value,charged_value,online_value-deduction, na.rm = TRUE))>0.0001)

# Michaelmas check
charged_tickets %>%
	filter(time %within% new_interval(dmy('26/6/2015'), dmy('9/12/2015'))) %>%
	group_by(crsid) %>%
	summarise(new_value = sum(price, na.rm=TRUE)) %>%
	full_join(charged_tickets %>%
							filter(event_id=='282') %>%
							group_by(crsid) %>%
							summarise(annual_dinner = sum(price, na.rm=TRUE))) %>%
	full_join(deductions %>%
							filter(time %within% new_interval(dmy('26/6/2015'), dmy('9/12/2015'))) %>%
							group_by(crsid) %>%
							summarise(deduction = sum(price, na.rm=TRUE))) %>%
	full_join(michaelmas_charged) %>%
	rename(charged_value = owes) %>%
	full_join(michaelmas_online) %>%
	rename(online_value = owes) %>%
	mutate(annual_dinner = replace(annual_dinner, is.na(annual_dinner), 0),
				 deduction = replace(deduction, is.na(deduction), 0)) %>%
	filter((pmax(charged_value,online_value-deduction-annual_dinner, na.rm = TRUE) - pmin(charged_value,online_value-deduction, na.rm = TRUE))>0.0001) %>%
	mutate(new_estimate = online_value-deduction-annual_dinner,
				 overcharge = charged_value-new_estimate)