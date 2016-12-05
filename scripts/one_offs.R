# mistaken charges, Michaelmas 2015
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

# Bill, Lent 2016
members <- read_csv('~/Downloads/MCR_Members_301115.csv') %>%
	rename(member = Member, name = Surname, crsid = CRSID) %>%
	mutate(crsid = tolower(crsid))

mistakes_from_michaelmas <- data_frame(crsid = c('alg67','nrvj2','cs787'), overcharge = c(18,54,18))

lent_2016_online <- read_tsv('~/Downloads/online_lent_2016.tsv') %>%
	transmute(crsid = str_to_lower(suid), owes = Owes)

deductions_lent_2016 <- deductions %>%
	filter(time %within% new_interval(dmy('9/12/2015'), dmy('13/3/2016'))) %>%
	group_by(crsid) %>%
	summarise(deduction = sum(price, na.rm=TRUE))

lent_2016_bill <- list(mistakes_from_michaelmas, lent_2016_online, deductions_lent_2016) %>% 
	purrr::reduce(full_join) %>% 
	arrange(crsid) %>% 
	select(crsid, owes, deduction, overcharge) %>%
	mutate_each(funs(replace(.,is.na(.),0)), owes, deduction, overcharge) %>%
	mutate(billed_amount = owes - deduction - overcharge) %>%
	filter(billed_amount!=0) %>%
	left_join(members) %>% select(-owes, -deduction, -overcharge)

