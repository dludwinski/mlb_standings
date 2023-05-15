#install.packages("rvest")
 
library(rvest)
require(httr)
library(sqldf)
#require(ggplot2)

#All_standings <- readRDS(url("https://github.com/dludwinski/mlb_standings/blob/main/All_standings.RDS?raw=true"))

AL_standing_tmp<-AL_standing_tmp[0,]
NL_standing_tmp<-NL_standing_tmp[0,]
Standings_raw<-AL_standing_tmp[0,]

#last run 2023-05-10
#StartDate<-as.Date('2023-05-06')
StartDate<- max(All_standings$date)+1
#EndDate<-as.Date('2000-10-25')
EndDate<-Sys.Date()


iDate<-StartDate

while (iDate<EndDate){
    print(iDate)
    
    year=format(iDate,"%Y")
    month=format(iDate,"%m")
    day=format(iDate,"%d")
    if (nrow(Standings_raw[Standings_raw$date==iDate,])==0){
        Sys.sleep(1.5)
        link <- paste0("https://www.baseball-reference.com/boxes/?year=",year,"&month=",month,"&day=",day)
        
        br_page <- read_html(link)
        
        AL_standing_tmp <- br_page %>% 
            html_node('#standings-upto-AL-overall') %>% 
            html_table()
        
        NL_standing_tmp <- br_page %>% 
            html_node('#standings-upto-NL-overall') %>% 
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


All_standings_staging<-Standings_raw

All_standings_staging$Division<-""
All_standings_staging$Current_Name<-""
All_standings_staging$Year<-as.numeric(format(All_standings_staging$date,"%Y"))

All_standings_staging$Games<-All_standings_staging$W+All_standings_staging$L
All_standings_staging$Year<-as.numeric(format(All_standings_staging$date,"%Y"))



#Add Spread 
All_standings_staging$spread<-All_standings_staging$W-All_standings_staging$L



#Current Names
All_standings_staging$Current_Name<-All_standings_staging$Tm
All_standings_staging[All_standings_staging$Tm=="MON",]$Current_Name<-"WSN"
All_standings_staging[All_standings_staging$Tm=="FLA",]$Current_Name<-"MIA"
All_standings_staging[All_standings_staging$Tm=="CAL",]$Current_Name<-"LAA"
All_standings_staging[All_standings_staging$Tm=="ANA",]$Current_Name<-"LAA"
All_standings_staging[All_standings_staging$Tm=="TBD",]$Current_Name<-"TBR"


#Divisions
#https://en.wikipedia.org/wiki/Timeline_of_Major_League_Baseball

#Old Divisions (1969-1994)
All_standings_staging[All_standings_staging$Year<1994 & All_standings_staging$Current_Name %in% c("WSN", "CHC", "STL", "PIT","NYM", "PHI"),]$Division <- "NL East"
All_standings_staging[All_standings_staging$Year<1994 & All_standings_staging$Current_Name %in% c("CIN", "ATL", "LAD", "SFG", "HOU", "SDP"),]$Division <- "NL West"
All_standings_staging[All_standings_staging$Year<1994 & All_standings_staging$Current_Name %in% c("MIL","BAL", "BOS", "NYY", "TOR", "CLE", "DET"),]$Division <- "AL East"
All_standings_staging[All_standings_staging$Year<1994 & All_standings_staging$Current_Name %in% c("MIN", "KCR", "CHW", "TEX",  "SEA", "OAK", "LAA"),]$Division <- "AL West"

#Division 1994-
All_standings_staging[All_standings_staging$Year>=1994 & All_standings_staging$Year<2013 & All_standings_staging$Current_Name %in% c("BAL", "BOS", "NYY", "TOR", "DET"),]$Division <- "AL East"
All_standings_staging[All_standings_staging$Year>=1994 & All_standings_staging$Year<2013 & All_standings_staging$Current_Name %in% c("CLE", "KCR", "CHW", "MIN", "MIL"),]$Division <- "AL Central"
All_standings_staging[All_standings_staging$Year>=1994 & All_standings_staging$Year<2013 & All_standings_staging$Current_Name %in% c("TEX", "HOU", "SEA", "LAA", "OAK"),]$Division <- "AL West"

All_standings_staging[All_standings_staging$Year>=1994 & All_standings_staging$Year<2013 & All_standings_staging$Current_Name %in% c("WSN", "NYM", "MIA", "PHI", "ATL"),]$Division <- "NL East"
All_standings_staging[All_standings_staging$Year>=1994 & All_standings_staging$Year<2013 & All_standings_staging$Current_Name %in% c("CHC", "STL", "PIT", "CIN"),]$Division <- "NL Central"
All_standings_staging[All_standings_staging$Year>=1994 & All_standings_staging$Year<2013 & All_standings_staging$Current_Name %in% c("LAD", "SFG", "COL", "ARI", "SDP"),]$Division <- "NL West"

#Division Nov 1997:Milwaukee Brewers moved from the AL to the NL Central
#Division Nov 1997:Detroit Tigers moved from the AL East to the AL Central
All_standings_staging[All_standings_staging$Year>=1998 & All_standings_staging$Year<2013 & All_standings_staging$Current_Name %in% c("MIL"),]$Division <- "NL Central"
All_standings_staging[All_standings_staging$Year>=1998 & All_standings_staging$Year<2013 & All_standings_staging$Current_Name %in% c("DET"),]$Division <- "AL Central"

#Division 1998: Expansion teams
All_standings_staging[All_standings_staging$Current_Name %in% c("TBR"),]$Division <- "AL East"
All_standings_staging[All_standings_staging$Current_Name %in% c("ARI"),]$Division <- "AL West"


# Current divisions (2013-???): Houston moved
All_standings_staging[All_standings_staging$Year>=2013 & All_standings_staging$Current_Name %in% c("BAL", "BOS", "NYY", "TBR", "TOR"),]$Division <- "AL East"
All_standings_staging[All_standings_staging$Year>=2013 & All_standings_staging$Current_Name %in% c("CLE", "DET", "KCR", "CHW", "MIN"),]$Division <- "AL Central"
All_standings_staging[All_standings_staging$Year>=2013 & All_standings_staging$Current_Name %in% c("TEX", "HOU", "SEA", "LAA", "OAK"),]$Division <- "AL West"

All_standings_staging[All_standings_staging$Year>=2013 & All_standings_staging$Current_Name %in% c("WSN", "NYM", "MIA", "PHI", "ATL"),]$Division <- "NL East"
All_standings_staging[All_standings_staging$Year>=2013 & All_standings_staging$Current_Name %in% c("CHC", "STL", "PIT", "MIL", "CIN"),]$Division <- "NL Central"
All_standings_staging[All_standings_staging$Year>=2013 & All_standings_staging$Current_Name %in% c("LAD", "SFG", "COL", "ARI", "SDP"),]$Division <- "NL West"





#Team colors
TeamColors<-read.csv("TeamColors.csv")

All_standings_staging<-sqldf("SELECT All_standings_staging.*, HexColor
      FROM All_standings_staging
      LEFT OUTER JOIN TeamColors
      on Team = All_standings_staging.Current_Name
")


#Fix endpoints
#All_standings_bckup<-All_standings

iyear<-1990

while (iyear<=2022){
    
    season_start<-min(All_standings_staging[All_standings_staging$W>1 & All_standings_staging$Year==iyear,]$date)-1
    season_games<-max(All_standings_staging[All_standings_staging$Year==iyear,]$Games)
    season_end<-min( All_standings_staging[All_standings_staging$Year==iyear & All_standings_staging$Games == season_games,]$date    )+1
    
    
    All_standings_staging <- subset(All_standings_staging, Year != iyear | (date>=season_start & date<=season_end ))
    iyear<-iyear+1
}



All_standings<-rbind(All_standings,All_standings_staging)

All_standings <- All_standings[!duplicated(All_standings), ]



saveRDS(All_standings, file = "All_standings.RDS") 

