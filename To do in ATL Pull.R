
library(rvest)
require(httr)
library(sqldf)

vp_link <- paste0("https://www.variety-playhouse.com/calendar/")

vp_br_page <- read_html(link)

        
        vp_info_tmp <- vp_br_page %>% 
            html_nodes(html,'#c-axs-event-card__title c-axs-event-card__title7133') %>% 
            html_name()
        
        NL_standing_tmp <- br_page %>% 
            html_nodes(html,'#standings-upto-NL-overall') %>% 
            html_table()
        
        AL_standing_tmp$date<-iDate
        NL_standing_tmp$date<-iDate
        
        #Insert dupcheck here
        
        Standings_raw <-rbind(Standings_raw, AL_standing_tmp, NL_standing_tmp)
        
        closeAllConnections()
    }
    
    iDate<-iDate+1
    if (format(iDate,"%m") > 10) {
        iDate<-iDate+150
    }
}



gc()
closeAllConnections()
