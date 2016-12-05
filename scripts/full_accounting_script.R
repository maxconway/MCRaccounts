# Full accounts

library(plyr)
library(dplyr)
library(gsheet)
library(stringr)
library(lubridate)
library(tidyr)

## General Prep
cash_book <- gsheet2tbl('https://docs.google.com/spreadsheets/d/1fyS4_Qoufdfy6kty5_qxuIDuvClIomn6E542vGwSF0Y/edit#gid=0') %>%
	mutate_each(funs(dmy), Incurred, Invoiced, Processed) %>%
	mutate(Amount = as.numeric(str_replace_all(Amount,'[^[:digit:].]*',''))) %>%
	mutate(delta = ifelse(Type=='income', Amount, -Amount))

bank_records <- gsheet2tbl('https://docs.google.com/spreadsheets/d/1bKBHDi12swl5DoRZaiRu3iBB18mgbi7D9W-vlte67Nk/edit?usp=sharing') %>%
	mutate_each(funs(str_replace_all(., '[^[:digit:].]*','')), Balance, Debit.Amount, Credit.Amount) %>%
	mutate_each(funs(as.numeric), Balance, Debit.Amount, Credit.Amount) %>%
	mutate(delta = pmax(Credit.Amount, -Debit.Amount, na.rm=TRUE)) %>%
	mutate(Transaction.Date = dmy(Transaction.Date))

## Join cash sheet and bank statement

### check for missing bank ids
if(!setequal(unique(cash_book$bank.ID), unique(bank_records$bank.ID))){
	stop('some missing bank ids')
}

### check for duplicated bank ids
cash_book %>%
	group_by(bank.ID) %>%
	filter(n()>1, is.finite(bank.ID)) # entries simply mean multiple records for one transaction

bank_records %>%
	group_by(bank.ID) %>%
	filter(n()>1, is.finite(bank.ID)) # payments made in multiple parts

### perform join
account <- left_join(cash_book, bank_records %>% filter(is.finite(bank.ID)), by='bank.ID')

### check that records match
account %>% 
	filter(is.finite(bank.ID)) %>%
	group_by(bank.ID) %>%
	summarise(delta.x = sum(delta.x), 
						delta.y=first(delta.y)
						) %>%
	mutate(error = delta.x-delta.y) %>%
	filter(error>0.00001) #for numeric errors

### Find bill dates


### Add a transfer date
account <- account %>%
	mutate(Transferred = ifelse(is.finite(Transaction.Date),
															Transaction.Date %>% as.character,
															Processed %>% as.character) %>% ymd)

college_bills <- account %>% 
	filter(str_detect(Description, '^College Charges for .+ Term$')) %>%
	getElement('Transferred') %>% sort 

account <- account %>%
	mutate(Transferred = ifelse(By=='bill',
															c(college_bills,NA)[findInterval(Processed, college_bills, rightmost.closed = TRUE)+1] %>% as.character,
															Processed %>% as.character) %>% ymd)

### Add an incurred date
account <- account %>%
	mutate(Incurred = ifelse(is.finite(Incurred),
													 Incurred %>% as.character,
													 Processed %>% as.character) %>% ymd)


## Cash flows: by date incurred
start = dmy('1/7/2014')
end = dmy('1/7/2015')

account %>%
	filter(Incurred >= start,
				 Incurred < end) %>%
	group_by(Category, Type) %>%
	summarise(total = sum(Amount)) %>% 
	spread(Type, total, 0) %>%
	mutate(net = income-expenditure)

## Balance sheet:
asof <- dmy('1/7/2015')

### bank account: Last bank statement before date
account %>% 
	filter(Transaction.Date<asof) %>%
	arrange(Transaction.Date) %>%
	summarise(Balance = last(Balance))
# note the bank doesn't explicitly tell us transaction order

### accounts receivable: type==income, incurred but not transfered
account %>%
	filter(Type=='income',
				 Incurred < asof,
				 Transferred >= asof | is.na(Transferred)) %>%
	select(Description, Incurred, Transferred, Amount)

### liabilites: type==expenditure, incurred but not transferred
account %>%
	filter(Type=='expenditure',
				 Incurred < asof,
				 Transferred >= asof | is.na(Transferred)) %>%
	select(Description, Incurred, Transferred, Amount)