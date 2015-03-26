# Create college bill
create_bill <- function(from, to){
	relevant <- new_interval(ymd(from), ymd(to))
	session <- html_session('http://www.selwynmcr.com/events/manage_events.php', set_cookies("Ucam-WebAuth-Session"="1!200!!20150321T132649Z!20150321T132649Z!7200!1426944408-31836-85!mjc233!!pwd!!1!eP.vXHsfEeEObBRvKb5Ngp0w22w_"))
	events_table <- session %>% 
		html_node('table') %>% 
		html_table(header=1)
	events_links <- session %>%
		html_nodes('tr') %>% Filter(x=., f= function(x){
			((x %>% html_nodes('td'))[[3]] %>% html_text()) == 'Confirmed'
		}) %>% html_node('a') %>% html_attr('href')
	newpages <- events_links %>% llply(function(link){session %>% jump_to(link)})
	relevant_pages <- newpages %>% Filter(x=., f=function(x){
		(x %>% html_node('#create_event_date') %>% html_attr('value') %>% ymd_hms()) %within% new_interval
	})
}