library(tidyverse)
library(stringr)
library(lubridate)

# This script creates a list of deductions from the online system in order to avoid double charging people who pay for dinners by their card's bar code.
# The database code is also appropriate as the base for any other kind of interaction with the online payment system.

# Event statuses:
# 1: confirmed
# 0: Live
# -1: cancelled

# first run in terminal:
# ssh -l <your_crsid> -L 9876:localhost:3306 shell.srcf.net
#
# - Requires srcf account with admin rights to selmcr
# - syntax of command is correct for linux 


selmcr <- src_mysql('selmcr', '127.0.0.1', 9876, 'selmcr', <password>)
# Password is listed in passwords spreadsheet

kevman <- c(
	'kevman_budget',
	'kevman_budget_privileges',
	'kevman_claims',
	'kevman_claimsets',
	'kevman_college_bills', # just whole bills w/clearing info
	'kevman_deposits', # looks like it might be tickets
	'kevman_event_options', # wisott
	'kevman_events', # wisott
	'kevman_expenses', # should no longer be used
	'kevman_interested', 
	'kevman_mail_log',
	'kevman_privileges',
	'kevman_purchases', # tickets
	'kevman_purchases_manual', # tickets
	'kevman_resell_waitinglist',
	'kevman_sql_log',
	'kevman_users' # wisott
	) %>%
	set_names(., str_replace(., '^kevman_','')) %>%
	map(partial(tbl, src=selmcr))

full_events <- kevman$events %>%
	select(event_id = id, event_status = status, price, ticketed, name, time)

full_tickets <- left_join(
	kevman$purchases %>%
		select(suid, purchase_id = id, event_id, event_option_id, ticket_status = status, message),
	kevman$event_options %>%
		select(event_option_id = id, event_id, option_price)
) 

ticketswithevents <- left_join(full_tickets, full_events) %>%
	collect %>%
	mutate(price_paid = ifelse(is.finite(option_price), option_price, price),
				 barcode = str_replace_all(message, '[Vv]egan|VEGAN','') %>% str_detect('(^|\\W)[vV][[:alnum:]]{4}(\\W|$)'),
				 dinner = str_detect(name, '[Ff]ormal Hall|MCR (Annual )?[dD]inner|^(MCR )?Annual Dinner$|^MCR.*[dD]inner$|MCR Burns Night Supper'),
				 deductable = barcode & dinner & (event_status %in% c(0,1)) & (ticket_status %in% c(0,2))) %>%
	select(-option_price, -price)

# create deductions list
ticketswithevents  %>%
	mutate(time = ymd_hms(time)) %>%
	filter(as.POSIXct(dmy('6/12/2016')) <= time, time < as.POSIXct(dmy('25/3/2017'))) %>% # filter dates here
	filter(!(event_id %in% c(347,346))) %>%
	filter(event_status %in% c(0,1), ticket_status %in% c(0,2)) %>%
	filter(deductable) %>%
	group_by(suid) %>%
	summarise(sum(price_paid)) %>%
	readr::write_tsv('temp/deductions.tsv')

# # base bills:
# ## doesn't deal with manual tickets
# ticketswithevents  %>%
# 	mutate(time = ymd_hms(time)) %>%
# 	filter() %>% # filter dates here
# 	filter(event_status == 1, ticket_status %in% c(0,2)) %>%
# 	group_by(suid, deductable) %>%
# 	summarise(sum(price_paid))
# 
# # bills derived from deposits:
# # not useful because not keyed against crsid
# kevman$deposits %>% 
# 	collect %>%
# 	mutate(ticket_type = str_extract(description, 'ticket type: [[:print:]]+?<br>') %>% 
# 				 	str_replace('^ticket type: ','') %>%
# 				 	str_replace('<br>$',''),
# 				 ticket_type = ifelse(is.na(ticket_type),'NA',ticket_type)
# 	) %>%
# 	mutate(manual = ticket_type=='manual')



