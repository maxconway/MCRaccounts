# Script used in working out charges for previous non-payers

previous_bills <- gsheet::gsheet2tbl('https://docs.google.com/spreadsheets/d/1x175-wPFCx0dfhO6xndMk67aj3llkRQYde6h9r9iHoY/edit?usp=sharing') %>%
	mutate(Payment = as.numeric(str_replace_all(Payment, '[£,]','')))

members <- gsheet::gsheet2tbl('https://docs.google.com/spreadsheets/d/189W6PmSiTNRgDYAE36sg8pP-DWPhXiZu_y2hFcJDfXM/edit?usp=sharing') %>%
	mutate(as.of=dmy(as.of)) %>%
	group_by(crsid) %>%
	filter(as.of==max(as.of))

debts <- previous_bills %>%
	group_by(crsid) %>%
	summarise(owes = - sum(Payment)) %>%
	filter(owes > 0) %>%
	left_join(members) %>%
	filter(Selwyn=='Yes' | is.na(Selwyn)) %>%
	select(crsid, Member.No, owes, Surname, Other.names)
	
debts %>% 
	mutate(owes = round(owes,2)) %>%
	readr::write_csv('temp/debts.csv') %>% 
	readr::write_tsv('temp/debts.tsv')