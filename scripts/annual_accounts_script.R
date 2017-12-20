library(gsheet)
library(dplyr)
library(stringr)
library(lubridate)
library(tidyr)
library(readr)

# Script to work out full annual accounts
# It's a bit byzantine, but covers every every base in terms of the types of the types of debts that could exist.

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
				 payment_date = if_else(paid, payment_date, as.Date(NA)),
				 processed_date = coalesce(Processed, payment_date)) %>%
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

# new summaries

year_start <- ymd_hms('2015/7/1 00:00:00')
year_end <- ymd_hms('2016/7/1 00:00:00')

relevant_accounts <- accounts %>%
	filter(pmin(accounting_date, processed_date, payment_date, na.rm = TRUE) < dmy('1/7/2016'),
				 pmax(accounting_date, processed_date, payment_date, na.rm = TRUE) >= dmy('1/7/2015'))

accounts %>%
	filter(accounting_date>dmy('1/1/2015')) %>%
	mutate(amount = delta,
				 status_at_end = ifelse(paid, 'paid', 'unpaid')) %>%
	select(-delta, -paid) %>%
	arrange(accounting_date, processed_date, payment_date) %>%
	write_tsv('summaries/all_transactions.tsv', na='')

bank_statement %>%
	filter(Transaction.Date>=dmy('1/7/2015')) %>%
	arrange(bank_order) %>%
	select(-bank_order) %>%
	write_tsv('summaries/bank_records.tsv', na='')

accounts %>%
	filter(!Category %in% c('Cash Accounting', 'Accounting')) %>%
	filter(accounting_date>=dmy('1/7/2015'), accounting_date<dmy('1/7/2016')) %>%
	arrange(accounting_date) %>%
	select(accounting_date, Description, with, Category, delta) %>%
	mutate(r=row_number()) %>%
	spread(Category, delta) %>%
	select(-r) %>%
	write_tsv('summaries/transactions_by_category.tsv', na='')

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

balance_sheet(dmy('1/7/2015')) %>%
	write_tsv('summaries/balance_sheet_start.tsv')

balance_sheet(dmy('1/7/2016')) %>%
	write_tsv('summaries/balance_sheet_end.tsv')


# monthly_balance_sheet.tsv
monthly_balance_sheet <- data_frame(start = seq(dmy('1/7/2015'), dmy('1/7/2016'), by='month'),
																		end = lead(start)) %>%
	filter(!is.na(end)) %>%
	purrr::pmap_df(function(start, end){
		start_balance_sheet <- balance_sheet(start)
		colnames(start_balance_sheet) <- paste0(colnames(start_balance_sheet), '_start')
		
		end_balance_sheet <- balance_sheet(end)
		colnames(end_balance_sheet) <- paste0(colnames(end_balance_sheet), '_end')
		
		bind_cols(cash_flow(start, end), start_balance_sheet, end_balance_sheet)
	}) %>%
	mutate(year = year(start), month = month(start, label=TRUE)) %>%
	select(-start, -end) %>%
	select(year, month, everything()) %>%
	mutate(total_assets_start = bank_balance_start + debtors_start - creditors_start - prepaid_income_start + prepaid_expenditure_start + cash_in_hand_start - processed_unpaid_expenditure_start + processed_unpaid_income_start +unprocessed_paid_expenditure_start - unprocessed_paid_income_start,
				 total_assets_end = bank_balance_end + debtors_end - creditors_end - prepaid_income_end + prepaid_expenditure_end + cash_in_hand_end - processed_unpaid_expenditure_end + processed_unpaid_income_end +unprocessed_paid_expenditure_end - unprocessed_paid_income_end)

monthly_balance_sheet %>%
	write_tsv('summaries/monthly_balance_sheet.tsv', na='')


# balance sheets
boundary_crossing <- relevant_accounts %>%
	mutate(accounted = case_when(
		.$accounting_date < date(year_start) ~ 'before',
		.$accounting_date >= date(year_start) & .$accounting_date < date(year_end) ~ 'during',
		.$accounting_date >= date(year_end) ~ 'after'),
		transferred = case_when(
			!.$paid ~ 'after',
			.$payment_date < date(year_start) ~ 'before',
			.$payment_date >= date(year_start) & .$payment_date < date(year_end) ~ 'during',
			.$payment_date >= date(year_end) ~ 'after'),
		processed = case_when(
			!.$paid ~ 'after',
			.$processed_date < date(year_start) ~ 'before',
			.$processed_date >= date(year_start) & .$processed_date < date(year_end) ~ 'during',
			.$processed_date >= date(year_end) ~ 'after')
	) %>%
	filter(!(accounted == transferred)) %>%
	filter(accounting_date > dmy('1/1/2015') | !is.na(bank.ID)) %>%
	arrange(accounted, transferred, sign(delta), accounting_date, payment_date) %>%
	select(accounting_date, processed_date, payment_date, amount = delta, everything()) 

unpaid_start <- boundary_crossing %>%
	filter(accounted=='before', processed=='during')

unpaid_end <- boundary_crossing %>%
	filter(accounted=='during', processed=='after')

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
	filter(accounted=='during', processed=='before')

prepaid_end <- boundary_crossing %>%
	filter(accounted=='after', processed=='during')

processed_unpaid_income_start <- boundary_crossing %>% filter(processed=='before', transferred %in% c('during', 'after')) %>% filter(amount>0)
processed_unpaid_expenditure_start <- boundary_crossing %>% filter(processed=='before', transferred %in% c('during', 'after')) %>% filter(amount<0) %>% mutate(amount = -amount)
unprocessed_paid_income_start <- boundary_crossing %>% filter(processed %in% c('during', 'after'), transferred=='before') %>% filter(amount>0)
unprocessed_paid_expenditure_start <- boundary_crossing %>% filter(processed %in% c('during', 'after'), transferred=='before') %>% filter(amount<0) %>% mutate(amount = -amount)

purrr::walk2(list(unpaid_start, unpaid_end, prepaid_start, prepaid_end, debtors_start, debtors_end, creditors_start, creditors_end, processed_unpaid_income_start, processed_unpaid_expenditure_start, unprocessed_paid_income_start, unprocessed_paid_expenditure_start),
						 c('unpaid_start', 'unpaid_end', 'prepaid_start', 'prepaid_end', 'debtors_start', 'debtors_end', 'creditors_start', 'creditors_end', 'processed_unpaid_income_start', 'processed_unpaid_expenditure_start', 'unprocessed_paid_income_start', 'unprocessed_paid_expenditure_start'),
						 function(df, name){
						 	df %>%
						 		mutate(transferred = if_else(paid, transferred, 'pending')) %>%
						 		arrange(accounted, transferred, sign(amount), Category, By, accounting_date, payment_date) %>%
						 		select(-paid) %>%
						 		write_tsv(paste0('summaries/',name,'.tsv'), na='')
						 })







