barcode_regex = '(^|\\W)[vV][[:alnum:]]{4}(\\W|$)'

library(readr)
library(dplyr)
library(stringr)
library(lubridate)
kevman_purchases <- read_csv2('~/Downloads/kevman_purchases.csv', col_types = 'ccccccc') %>%
	rename(purchases_id = id, purchase_status = status)
kevman_purchases_manual <- read_csv2('~/Downloads/kevman_purchases_manual.csv', col_types = 'cccci') %>%
	rename(manual_purchase_id = id)
kevman_events <- read_csv2('~/Downloads/kevman_events.csv', col_types = 'cccccdicccicccc') %>%
	rename(event_id = id, event_status=status) %>%
	mutate(time = ymd_hms(time))
kevman_event_options <- read_csv2('~/Downloads/kevman_event_options.csv', col_types = 'cccd') %>%
	rename(event_option_id = id)
kevman_deposits <- read_csv2('~/Downloads/kevman_deposits.csv', col_types = 'cccccdcccccc')
#kevman_users <- read_csv2('~/Downloads/kevman_users.csv')
kevman_college_bills <- read_csv2('~/Downloads/kevman_college_bills.csv', col_types = 'ccccc')


# simplified form
kevman_events %>% select(event_id, name, event_status)
kevman_event_options %>% select(event_id, event_option_id, option_name)
kevman_purchases %>% select(purchases_id, event_id, event_option_id, purchase_status) %>%
	group_by(purchase_status) %>%
	do(head(.))

# table of every ticket
left_join(left_join(kevman_purchases, kevman_event_options,  by='event_option_id') %>% 
						select(-event_id.y, event_id = event_id.x) %>%
						bind_rows(kevman_purchases_manual %>% select(manual_cause=description, option_price=price)), 
					kevman_events, by='event_id') %>%
	select(-tickets_start, -tickets_end, -description, -bought_at) %>%
	mutate(price = ifelse(is.na(option_price), price, option_price)) -> all_tickets


# deductions
all_tickets %>%
	filter(event_status == '1', purchase_status %in% c('0','2')) %>%
	select(crsid = suid, price, message, time, name, event_id) %>%
	mutate(barcode = str_detect(message, '(^|\\W)[vV][[:alnum:]]{4}(\\W|$)') & !str_detect(message, '[Vv]egan|VEGAN'),
				 dinner = str_detect(name, '[Ff]ormal Hall|MCR [dD]inner|^Annual Dinner$|^MCR.*[dD]inner$')) %>%
	filter(barcode & dinner) %>%
	select(-message, -barcode, -dinner) -> deductions

# charged tickets
all_tickets %>%
	filter(event_status == '1', purchase_status %in% c('0','2')) %>%
	select(crsid = suid, price, message, time, name, event_id) %>%
	mutate(barcode = str_detect(message, '(^|\\W)[vV][[:alnum:]]{4}(\\W|$)') & !str_detect(message, '[Vv]egan|VEGAN'),
				 dinner = str_detect(name, '[Ff]ormal Hall|MCR [dD]inner|^Annual Dinner$|^MCR.*[dD]inner$')) %>%
	filter(!(barcode & dinner)) %>%
	select(-message, -barcode, -dinner) -> charged_tickets
	