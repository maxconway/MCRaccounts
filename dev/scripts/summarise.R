library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)

# Read
cash_raw <- gsheet2tbl('https://docs.google.com/spreadsheets/d/1fyS4_Qoufdfy6kty5_qxuIDuvClIomn6E542vGwSF0Y/edit')

cash <- cash_raw %>%
	mutate(Incurred = dmy(Incurred),
				 Invoiced = dmy(Invoiced),
				 Processed = dmy(Processed),
				 Amount = as.numeric(str_replace_all(Amount,',','')),
				 Description = str_replace(Description, '^Personal Development Subsidy.*', 'Personal Development Subsidy'),
				 Description = str_replace(Description, '^Friday night drinks.*', 'Friday Night Drinks')
				 )

# Expenditure summary
cash %>%
	filter(Processed >= dmy('1/1/2015')) %>% # Date range
	filter(Type=='expenditure') %>%
	filter(!(Category %in% c('Swaps', 'Dinners/College Charges')),
				 !str_detect(Description, 'edinburgh trip')) %>% # Money in debit
	ggplot(aes(x=Category, y=Amount, fill=Description)) + 
	geom_bar(stat='identity', position='stack') + 
	ylab('Spending (£)') + 
	coord_flip()

# Expenditure total
cash %>%
	filter(Processed >= dmy('1/1/2015')) %>% # Date range
	filter(Type=='expenditure') %>%
	filter(!(Category %in% c('Swaps', 'Dinners/College Charges')),
				 !str_detect(Description, 'edinburgh trip')) %>%
	summarise(total = sum(Amount))