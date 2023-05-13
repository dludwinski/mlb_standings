view_in_excel <- function(tablename) {
    tmp_a<-paste("tmp_Exports\\",deparse(substitute(tablename)),".csv",sep="")
    write.csv(tablename,tmp_a, row.names = TRUE)
    shell(tmp_a, wait=FALSE)
}


#Ad hoc shit



#Fix some colors
All_standings[All_standings$Tm %in% c("ATL"),]$HexColor <- "#CE1F43"
TeamColors[TeamColors$Team %in% c("ATL"),]$HexColor <- "#CE1F43"



All_standings<-sqldf("SELECT Tm, W, L, `W-L%`, GB, RS, RA, `pythW-L%`, date, spread, All_standings.Division, Games, Year, Current_Name, TeamColors.HexColor
      FROM All_standings
      LEFT OUTER JOIN TeamColors
      on Team = All_standings.Current_Name
")
#All_standings$HexColor<-paste0("#",All_standings$HexColor)





sqldf("SELECT min(spread), max(spread), count(distinct date) 
      FROM All_standings
")

sqldf("SELECT min(date), max(date), count(distinct date) 
      FROM All_standings
")
sqldf("SELECT min(date), max(date), count(distinct date) 
      FROM All_standings_staging
")

All_standings$HexColor<-paste0("#",All_standings$HexColor)

if (year=="2010-2019"){yearmin<-2010}


teams2<-sqldf("SELECT distinct Tm, Current_Name, Max(date), min(date), count(*)
      FROM All_standings
      group by Tm,Current_Name
")
view_in_excel(teams2)



year_days<-sqldf("SELECT Year, min(date), max(date), count(distinct date), max(games)
      FROM All_standings
      GROUP BY Year
")
view_in_excel(year_days)





All_standings <- subset(All_standings, select = -c(HexColor))


#Team colors
TeamColors<-read.csv("TeamColors.csv")

All_standings<-sqldf("SELECT All_standings.*, HexColor
      FROM All_standings
      LEFT OUTER JOIN TeamColors
      on Team = All_standings.Current_Name
")


