library(plyr)
library(dplyr)
library(lubridate)
library(stringr)
library(readr)
library(tidyr)

income <- read_csv('downloaded/Income-13-14.csv') %>% 
	gather(Category, Amount, -c(1:2), na.rm = TRUE) %>%
	mutate(Date = dmy(Date),
				 Amount = as.numeric(Amount),
				 Type='income'
				 )
expenditure <- read_csv('downloaded/Income-13-14.csv') %>% 
	gather(Category, Amount, -c(1:2), na.rm = TRUE) %>%
	mutate(Date = dmy(Date),
				 Amount = as.numeric(Amount),
				 Type='expenditure'
	)
all <- rbind_list(income, expenditure)

write_csv(all, './downloaded/accounts-13-14.csv')
