library(gsheet)
library(dplyr)
library(stringr)
library(lubridate)
library(tidyr)
library(readr)

# Read and clean sheets

accounts <- gsheet2tbl('https://docs.google.com/spreadsheets/d/1fyS4_Qoufdfy6kty5_qxuIDuvClIomn6E542vGwSF0Y/edit#gid=0') %>%
	select(Description : bank.ID) %>%
	mutate_each(funs(dmy), Incurred, Invoiced, Processed) %>%
	mutate(Amount = Amount %>%
				 	str_replace_all('£|,','') %>%
				 	as.numeric,
				 delta = Amount * ((Type=='income') - (Type=='expenditure'))) %>%
	select(-Amount, -Type) %>%
	mutate(paid = (!is.na(bank.ID)) | (By %in% c('cash', 'other', 'bill')))

bank_statement <- gsheet2tbl('https://docs.google.com/spreadsheets/d/1bKBHDi12swl5DoRZaiRu3iBB18mgbi7D9W-vlte67Nk/edit#gid=0') %>%
	mutate_each(funs(dmy), Transaction.Date) %>%
	mutate_each(funs((. %>%
											str_replace_all('£|,','') %>%
											as.numeric )), Debit.Amount, Credit.Amount, Balance) %>%
	mutate(account_gain = coalesce(Credit.Amount, - Debit.Amount)) %>%
	select(bank.ID, bank_order, Transaction.Date, Transaction.Description, Balance, account_gain)

## clean up dates
accounts <- bank_statement %>%
	filter(!is.na(bank.ID)) %>%
	group_by(bank.ID) %>%
	summarise(bank_date = max(Transaction.Date)) %>%
	right_join(accounts) %>%
	mutate(payment_date = coalesce(bank_date, Processed, Invoiced, Incurred),
				 accounting_date = coalesce(Incurred, Invoiced, Processed, Processed, bank_date),
				 payment_date = if_else(paid, payment_date, as.Date(NA))) %>%
	select(-bank_date, -Incurred, -Invoiced, -Processed)

# checks
## Find transactions where bank and accounts don't match
full_join(accounts %>%
					 	filter(!is.na(bank.ID)) %>%
					 	group_by(bank.ID) %>%
					 	summarise(account_transaction = sum(delta, na.rm=TRUE)) %>%
						filter(!near(account_transaction,0)),
					 bank_statement %>%
					 	filter(!is.na(bank.ID)) %>%
					 	group_by(bank.ID) %>%
					 	summarise(bank_transaction = sum(account_gain, na.rm=TRUE)) %>%
						filter(!near(bank_transaction,0))) %>%
	filter(!near(account_transaction, bank_transaction) | is.na(bank_transaction) | is.na(account_transaction))

# create annual summaries
year_start <- ymd_hms('2015/7/1 00:00:00')
year_end <- ymd_hms('2016/7/1 00:00:00')
year <- interval(year_start, year_end)

relevant_accounts <- accounts %>%
	filter(payment_date %within% year | accounting_date %within% year)

# banking_transactions_summary:
relevant_accounts %>% 
	filter(paid) %>%
	filter(payment_date %within% year) %>%
	left_join(bank_statement %>%
							filter(!is.na(bank.ID))
						) %>%
	select(Transaction.Date, Transaction.Description, bank.ID, Description, with, Category, delta) %>%
	tibble::rownames_to_column() %>%
	spread(Category, delta, fill=0) %>%
	select(-rowname) %>%
	arrange(Transaction.Date) %>%
	write_tsv('summaries/banking_transactions_summary.tsv')

# banking_monthly_summary:
banking_monthly_summary <- bank_statement %>%
	filter(Transaction.Date %within% year) %>%
	group_by(year = year(Transaction.Date), month = month(Transaction.Date, label=TRUE)) %>%
	arrange(bank_order) %>%
	summarise(starting_balance = first(Balance) - first(account_gain),
						ending_balance = last(Balance),
						total_income = sum(pmax(account_gain, 0)),
						total_expenditure = -sum(pmin(account_gain, 0)))

banking_monthly_summary %>%
	write_tsv('summaries/banking_monthly_summary.tsv')

# boundary crossing:
boundary_crossing <- accounts %>%
	mutate(accounted = case_when(
		.$accounting_date < date(year_start) ~ 'before',
		.$accounting_date >= date(year_start) & .$accounting_date < date(year_end) ~ 'during',
		.$accounting_date >= date(year_end) ~ 'after'),
		transferred = case_when(
			!.$paid ~ 'after',
			.$payment_date < date(year_start) ~ 'before',
			.$payment_date >= date(year_start) & .$payment_date < date(year_end) ~ 'during',
			.$payment_date >= date(year_end) ~ 'after')
		) %>%
	filter(!(accounted == transferred)) %>%
	filter(accounting_date > dmy('1/1/2015') | !is.na(bank.ID)) %>%
	arrange(accounted, transferred, sign(delta), accounting_date, payment_date) %>%
	select(accounting_date, payment_date, amount = delta, everything()) 


# checks
balance_at_start <- bank_statement %>%
	arrange(bank_order) %>%
	filter(Transaction.Date < date(year_start)) %>%
	getElement('Balance') %>%
	last()

balance_at_end <- bank_statement %>%
	arrange(bank_order) %>%
	filter(Transaction.Date < date(year_end)) %>%
	getElement('Balance') %>%
	last()

payments_during_period <- accounts %>%
	filter(payment_date >= date(year_start), payment_date < date(year_end)) %>%
	filter(paid) %>%
	summarise(total = sum(delta)) %>%
	.$total

round(balance_at_end - balance_at_start - payments_during_period)

unpaid_start <- boundary_crossing %>%
	filter(accounted=='before', transferred=='during')

unpaid_end <- boundary_crossing %>%
	filter(accounted=='during', transferred=='after')

debtors_start <- unpaid_start %>%
	filter(amount>0)

creditors_start <- unpaid_start %>%
	filter(amount<0) %>%
	mutate(amount = -amount)

debtors_end <- unpaid_end %>%
	filter(amount>0)

creditors_end <- unpaid_end %>%
	filter(amount<0) %>%
	mutate(amount = -amount)

prepaid_start <- boundary_crossing %>%
	filter(accounted=='during', transferred=='before')

prepaid_end <- boundary_crossing %>%
	filter(accounted=='after', transferred=='during')
	
	
total_at_start <- balance_at_start + sum(unpaid_start$amount) - sum(prepaid_start$amount)
total_at_end <- balance_at_end + sum(unpaid_end$amount) - sum(prepaid_end$amount)

change_during_year <- accounts %>%
	filter(accounting_date >= date(year_start), accounting_date < date(year_end)) %>%
	summarise(total = sum(delta)) %>%
	getElement('total')

round(total_at_end-total_at_start - change_during_year,2)

# write out tables
purrr::walk2(list(unpaid_start, unpaid_end, prepaid_start, prepaid_end, debtors_start, debtors_end, creditors_start, creditors_end),
			c('unpaid_start', 'unpaid_end', 'prepaid_start', 'prepaid_end', 'debtors_start', 'debtors_end', 'creditors_start', 'creditors_end'),
			function(df, name){
				df %>%
					mutate(transferred = if_else(paid, transferred, 'pending')) %>%
					arrange(accounted, transferred, sign(amount), Category, By, accounting_date, payment_date) %>%
					select(-paid) %>%
					write_tsv(paste0('summaries/',name,'.tsv'), na='')
			})

# monthly summary
monthly_balance_sheet <- data_frame(start = seq(dmy('1/7/2015'), dmy('1/7/2016'), by='month'),
					 end = lead(start)) %>%
	filter(!is.na(end)) %>%
	purrr::pmap_df(function(start, end){
		result <- data_frame(start=as.Date(start, '1970-01-01 00:00.00 UTC'), end=as.Date(end, '1970-01-01 00:00.00 UTC'))
		
		relevant_accounts_without_cash_accounting <- relevant_accounts %>% filter(Category!='Cash Accounting')
		
		result$starting_bank_balance = bank_statement %>%
			filter(Transaction.Date < start) %>%
			filter(bank_order==max(bank_order)) %>%
			.$Balance %>% round(2)
		
		result$ending_bank_balance = bank_statement %>%
			filter(Transaction.Date < end) %>%
			filter(bank_order==max(bank_order)) %>%
			.$Balance %>% round(2)
		
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
		
		result$debtors_start = relevant_accounts_without_cash_accounting %>%
			filter((payment_date >= start | !paid), accounting_date < start) %>%
			filter(delta>0) %>%
			summarise(amount = sum(delta)) %>%
			.$amount %>% round(2)
		
		result$creditors_start = relevant_accounts_without_cash_accounting %>%
			filter((payment_date >= start | !paid), accounting_date < start) %>%
			filter(delta<0) %>%
			summarise(amount = -sum(delta)) %>%
			.$amount %>% round(2)
		
		result$debtors_end = relevant_accounts_without_cash_accounting %>%
			filter((payment_date >= end | !paid), accounting_date < end) %>%
			filter(delta>0) %>%
			summarise(amount = sum(delta)) %>%
			.$amount %>% round(2)
		
		result$creditors_end = relevant_accounts_without_cash_accounting %>%
			filter((payment_date >= end | !paid), accounting_date < end) %>%
			filter(delta<0) %>%
			summarise(amount = -sum(delta)) %>%
			.$amount %>% round(2)
		
		result$prepaid_income_start = relevant_accounts_without_cash_accounting %>%
			filter((payment_date < start), accounting_date >= start) %>%
			filter(delta>0) %>%
			summarise(amount = sum(delta)) %>%
			.$amount %>% round(2)
		
		result$prepaid_expenditure_start = relevant_accounts_without_cash_accounting %>%
			filter((payment_date < start), accounting_date >= start) %>%
			filter(delta<0) %>%
			summarise(amount = -sum(delta)) %>%
			.$amount %>% round(2)
		
		result$prepaid_income_end = relevant_accounts_without_cash_accounting %>%
			filter((payment_date < end), accounting_date >= end) %>%
			filter(delta>0) %>%
			summarise(amount = sum(delta)) %>%
			.$amount %>% round(2)
		
		result$prepaid_expenditure_end = relevant_accounts_without_cash_accounting %>%
			filter((payment_date < end), accounting_date >= end) %>%
			filter(delta<0) %>%
			summarise(amount = -sum(delta)) %>%
			.$amount %>% round(2)
		
		result$cash_in_hand_start = accounts %>%
			filter(Category=='Cash Accounting') %>%
			filter(accounting_date < start) %>%
			summarise(amount =-sum(delta)) %>%
			.$amount %>% round(2)
		
		result$cash_in_hand_end = accounts %>%
			filter(Category=='Cash Accounting') %>%
			filter(accounting_date < end) %>%
			summarise(amount =-sum(delta)) %>%
			.$amount %>% round(2)
		
		result
	}) %>%
	mutate(year = year(start), month = month(start, label=TRUE)) %>%
	select(-start, -end) %>%
	select(year, month, everything()) %>%
	mutate(total_assets_start = starting_bank_balance + debtors_start - creditors_start - prepaid_income_start + prepaid_expenditure_start + cash_in_hand_start,
				 total_assets_end = ending_bank_balance + debtors_end - creditors_end - prepaid_income_end + prepaid_expenditure_end + cash_in_hand_end)

monthly_balance_sheet %>%
	write_tsv('summaries/monthly_balance_sheet.tsv', na='')

# new tables
relevant_accounts %>%
	mutate(amount = delta,
				 status_at_end = ifelse(paid, 'paid', 'unpaid')) %>%
	select(-delta, -paid) %>%
	arrange(accounting_date, payment_date) %>%
	write_tsv('summaries/all_relevant_transactions.tsv', na='')

bank_statement %>%
	filter(Transaction.Date>=dmy('1/7/2015'), Transaction.Date<dmy('1/7/2016')) %>%
	arrange(bank_order) %>%
	select(-bank_order) %>%
	write_tsv('summaries/bank_records_for_year.tsv', na='')

accounts %>%
	filter(!Category %in% c('Cash Accounting', 'Accounting')) %>%
	filter(accounting_date>=dmy('1/7/2015'), accounting_date<dmy('1/7/2016')) %>%
	arrange(accounting_date) %>%
	select(accounting_date, Description, with, Category, delta) %>%
	mutate(r=row_number()) %>%
	spread(Category, delta) %>%
	select(-r) %>%
	write_tsv('summaries/transactions_by_category.tsv', na='')

# overviews
accounts %>%
	filter(!Category %in% c('Cash Accounting', 'Accounting')) %>%
	filter(accounting_date>=dmy('1/7/2015'), accounting_date<dmy('1/7/2016')) %>%
	arrange(accounting_date) %>%
	select(accounting_date, Description, with, Category, delta) %>%
	filter(delta>0) %>%
	group_by(Category) %>%
	summarise(total = sum(delta)) %>%
	write_tsv('summaries/income_overview.tsv')

accounts %>%
	filter(!Category %in% c('Cash Accounting', 'Accounting')) %>%
	filter(accounting_date>=dmy('1/7/2015'), accounting_date<dmy('1/7/2016')) %>%
	arrange(accounting_date) %>%
	select(accounting_date, Description, with, Category, delta) %>%
	filter(delta<0) %>%
	group_by(Category) %>%
	summarise(total = -sum(delta)) %>%
	write_tsv('summaries/expenditure_overview.tsv')