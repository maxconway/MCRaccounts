# download a bill

download_bill <- function(url, cookie){
	session <- html_session(url, set_cookies("Ucam-WebAuth-Session"=cookie))
	bill_table <- session %>% 
		html_node('table') %>% 
		html_table(header=1)
	return(bill_table)
}