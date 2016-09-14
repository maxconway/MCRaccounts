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

accounts %>%
	filter(accounting_date > dmy('1/1/2015')) %>%
	filter(delta<0) %>%
	group_by(month=floor_date(accounting_date, 'month'), Category, type=c('income','expenditure')[(delta<0)+1]) %>%
	summarise(amount = sum(abs(delta))) %>%
	ggplot(aes(x=month, fill=Category, y=amount)) + geom_bar(stat='identity')

balances %>%
	mutate(assets = assets-balances[match(floor_date(date,'year'), balances$date),][['assets']]) %>%
	ggplot(aes(x=date-floor_date(date, 'year'), y=assets, colour=factor(floor_date(date, 'year')))) + 
	geom_path()
