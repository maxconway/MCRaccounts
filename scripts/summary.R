library(gsheet)
library(lubridate)
library(stringr)

full <- gsheet2tbl('https://docs.google.com/spreadsheets/d/1fyS4_Qoufdfy6kty5_qxuIDuvClIomn6E542vGwSF0Y/edit#gid=0')

full %>%
  select(Description : Type) %>%
  mutate_each(funs(dmy), Incurred, Invoiced, Processed) %>%
  mutate(proxy_date = as.Date(origin = dmy('1/1/1970'), x=ifelse(!is.na(Incurred), Incurred, ifelse(is.na(Processed) | Processed > Invoiced, Invoiced, Processed)))) %>%
  select(-Incurred, -Invoiced, -Processed) %>%
  mutate(Amount = as.numeric(str_replace_all(Amount, '£|,', ''))) -> parsed

# this year
parsed %>%
  filter(dmy('1/1/2016') <= proxy_date) %>%
  filter(!(Category %in% c('Dinners/College Charges', 'Swaps'))) -> relevant

relevant %>%
  filter(str_detect(Category, 'Entertainment')) %>%
  View

relevant %>%
  group_by(Type, Category) %>%
  summarise(sum(Amount))

# last year
parsed %>%
  filter(dmy('1/1/2015') <= proxy_date) %>%
  filter(dmy('24/5/2015') > proxy_date) %>%
  filter(!(Category %in% c('Dinners/College Charges', 'Swaps'))) -> relevant

relevant %>%
  filter(str_detect(Category, 'Entertainment')) %>%
  View