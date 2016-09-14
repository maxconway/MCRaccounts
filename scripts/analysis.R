# analysis
library(ggplot2)

balances <- data_frame(date = seq.Date(dmy('1/1/2015'),today(), by='day'),
											 balance = purrr::map(date, balance_sheet)) %>%
	unnest(balance) %>%
	mutate(assets = bank_balance + 
				 	cash_in_hand + 
				 	debtors - creditors + 
				 	prepaid_expenditure - prepaid_income + 
				 	processed_unpaid_income - processed_unpaid_expenditure +
				 	unprocessed_paid_expenditure - unprocessed_paid_income)

balances %>%
	ggplot(aes(x=date, y=assets)) + 
	geom_path()

balances %>%
	ggplot(aes(x=date-floor_date(date, 'year'), y=assets, colour=factor(floor_date(date, 'year')))) + 
	geom_path()

balances %>%
	group_by(yday(date)) %>%
	filter(n()>1) %>%
	group_by(year = year(date)) %>%
	summarise(median(assets))

