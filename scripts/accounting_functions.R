# accounting functions
balance_sheet <- function(at){
	relevant_accounts_without_cash_accounting <- relevant_accounts %>% filter(Category!='Cash Accounting')
	
	bank_balance = bank_statement %>%
		filter(Transaction.Date < at) %>%
		filter(bank_order==max(bank_order)) %>%
		.$Balance %>% round(2)
	
	cash_in_hand = accounts %>%
		filter(Category=='Cash Accounting') %>%
		filter(accounting_date < at) %>%
		summarise(amount =-sum(delta)) %>%
		.$amount %>% round(2)
	
	debtors = relevant_accounts_without_cash_accounting %>%
		filter((processed_date >= at | !paid), accounting_date < at) %>%
		filter(delta>0) %>%
		summarise(amount = sum(delta)) %>%
		.$amount %>% round(2)
	
	creditors = relevant_accounts_without_cash_accounting %>%
		filter((processed_date >= at | !paid), accounting_date < at) %>%
		filter(delta<0) %>%
		summarise(amount = -sum(delta)) %>%
		.$amount %>% round(2)
	
	prepaid_income = relevant_accounts_without_cash_accounting %>%
		filter((processed_date < at), accounting_date >= at) %>%
		filter(delta>0) %>%
		summarise(amount = sum(delta)) %>%
		.$amount %>% round(2)
	
	prepaid_expenditure = relevant_accounts_without_cash_accounting %>%
		filter((processed_date < at), accounting_date >= at) %>%
		filter(delta<0) %>%
		summarise(amount = -sum(delta)) %>%
		.$amount %>% round(2)
	
	processed_unpaid_expenditure = relevant_accounts_without_cash_accounting %>%
		filter(processed_date < at, payment_date >= at) %>%
		filter(delta<0) %>%
		summarise(amount = -sum(delta)) %>%
		.$amount %>% round(2)
	
	processed_unpaid_income = relevant_accounts_without_cash_accounting %>%
		filter(processed_date < at, payment_date >= at) %>%
		filter(delta>0) %>%
		summarise(amount = sum(delta)) %>%
		.$amount %>% round(2)
	
	unprocessed_paid_expenditure = relevant_accounts_without_cash_accounting %>%
		filter(processed_date >= at, payment_date < at) %>%
		filter(delta<0) %>%
		summarise(amount = -sum(delta)) %>%
		.$amount %>% round(2)
	
	unprocessed_paid_income = relevant_accounts_without_cash_accounting %>%
		filter(processed_date >= at, payment_date < at) %>%
		filter(delta>0) %>%
		summarise(amount = sum(delta)) %>%
		.$amount %>% round(2)
	
	result <- data_frame(bank_balance,
											 cash_in_hand,
											 debtors,
											 creditors,
											 prepaid_income,
											 prepaid_expenditure,
											 processed_unpaid_expenditure,
											 processed_unpaid_income,
											 unprocessed_paid_expenditure,
											 unprocessed_paid_income
											 )
	
	return(result)
}

cash_flow <- function(start, end){
	result <- data_frame(start=as.Date(start, '1970-01-01 00:00.00 UTC'), end=as.Date(end, '1970-01-01 00:00.00 UTC'))
	
	relevant_accounts_without_cash_accounting <- relevant_accounts %>% 
		filter(!Category %in% c('Cash Accounting', 'Accounting'))
	
	result$bank_credits = bank_statement %>%
		filter(Transaction.Date >= start, Transaction.Date < end) %>%
		filter(account_gain>0) %>%
		summarise(amount = sum(account_gain)) %>%
		.$amount %>% round(2)
	
	result$bank_debits = bank_statement %>%
		filter(Transaction.Date >= start, Transaction.Date < end) %>%
		filter(account_gain<0) %>%
		summarise(amount = -sum(account_gain)) %>%
		.$amount %>% round(2)
	
	result$income = relevant_accounts_without_cash_accounting %>%
		filter(accounting_date >= start, accounting_date < end) %>%
		filter(delta>0) %>%
		summarise(amount = sum(delta)) %>%
		.$amount %>% round(2)
	
	result$expenditure = relevant_accounts_without_cash_accounting %>%
		filter(accounting_date >= start, accounting_date < end) %>%
		filter(delta<0) %>%
		summarise(amount = -sum(delta)) %>%
		.$amount %>% round(2)
	
	return(result)
}