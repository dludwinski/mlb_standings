library(shiny)
library(sqldf)
#library(ggplot2)

All_standings <- readRDS(url("https://github.com/dludwinski/mlb_standings/blob/main/All_standings.RDS?raw=true"))


function(input, output) {
    
    output$AL_East_Graph<- renderPlot({
        year <- input$sel_year
        Division <- "AL East"
        yearmin<-as.numeric(input$max_year)-as.numeric(input$number_of_years)+1
        yearmax<-as.numeric(input$max_year)
        
        
        plot_temp<-All_standings[All_standings$Division==Division & All_standings$Year>=yearmin & All_standings$Year<=yearmax ,]
        
        team_division_colors<-sqldf("SELECT Current_Name, HexColor, Max(W) as MaxW from plot_temp group by Current_Name")
        team_division_colors<-team_division_colors[order(-team_division_colors$MaxW),]
        
        MaxSpread<-max(All_standings[All_standings$Year>=yearmin & All_standings$Year<=yearmax,]$spread)
        MinSpread<-min(All_standings[All_standings$Year>=yearmin & All_standings$Year<=yearmax,]$spread)
        StartDate<-max(All_standings[All_standings$Year>=yearmin & All_standings$Year<=yearmax,]$date)
        EndDate<-min(All_standings[All_standings$Year>=yearmin & All_standings$Year<=yearmax,]$date)
        
        grph_plot_title <- Division
         
        par(mar = c(5, 5, 5, 15), xpd=TRUE )
        plot(plot_temp$date, plot_temp$spread
             , main= grph_plot_title, cex.main = 2.5
             , pch = 16
             , col=plot_temp$HexColor
             , cex.lab = 1.7, xlab="", ylab="Net Wins"
             , plot.window(xlim=c(EndDate,StartDate), ylim=c(MinSpread,MaxSpread), xaxs="i", yaxs="i") #
             #, cex.axis	= 1.1
             #    , log="x"
        )
        #grid(nx = NULL, ny = NULL, col = "lightgray", lty = 1,lwd = par("lwd"), equilogs = FALSE)
        legend(StartDate, MaxSpread
               , legend = team_division_colors[,"Current_Name"]
               , col=team_division_colors[,"HexColor"]
               , lty=1, lwd=8, cex=1.2
               ,box.lty=1)
        
    })
    

    output$AL_Central_Graph<- renderPlot({
        
        yearmax<-as.numeric(input$max_year)
        if (yearmax>1993){
            
            yearmin<-as.numeric(input$max_year)-as.numeric(input$number_of_years)+1
            
            
            MaxSpread<-max(All_standings[All_standings$Year>=yearmin & All_standings$Year<=yearmax,]$spread)
            MinSpread<-min(All_standings[All_standings$Year>=yearmin & All_standings$Year<=yearmax,]$spread)
            StartDate<-max(All_standings[All_standings$Year>=yearmin & All_standings$Year<=yearmax,]$date)
            EndDate<-min(All_standings[All_standings$Year>=yearmin & All_standings$Year<=yearmax,]$date)
            
            plot_AL_Central<-All_standings[All_standings$Division=="AL Central" & All_standings$Year>=yearmin & All_standings$Year<=yearmax,]
            
            team_division_colors<-sqldf("SELECT distinct Current_Name, HexColor, Max(W) as MaxW from plot_AL_Central group by Current_Name")
            team_division_colors<-team_division_colors[order(-team_division_colors$MaxW),]
            
            grph_plot_title <- "AL Central"
            
            par(mar = c(5, 5, 5, 15), xpd=TRUE )
            plot(plot_AL_Central$date, plot_AL_Central$spread
                 , main= grph_plot_title, cex.main = 2.5
                 , pch = 16
                 , col=plot_AL_Central$HexColor
                 , cex.lab = 1.7, xlab="", ylab="Net Wins"
                 , plot.window(xlim=c(EndDate,StartDate), ylim=c(MinSpread,MaxSpread), xaxs="i", yaxs="i") #
                 #, cex.axis	= 1.1
                 #    , log="x"
            )
            #grid(nx = NULL, ny = NULL, col = "lightgray", lty = 1,lwd = par("lwd"), equilogs = FALSE)
            legend(StartDate, MaxSpread
                   #grph_min_date, 40
                   , legend = team_division_colors[,"Current_Name"]
                   , col=team_division_colors[,"HexColor"]
                   , lty=1, lwd=8, cex=1.2
                   ,box.lty=1)
        }
    })
    
    
    output$AL_West_Graph<- renderPlot({
        yearmin<-as.numeric(input$max_year)-as.numeric(input$number_of_years)+1
        yearmax<-as.numeric(input$max_year)
        
        MaxSpread<-max(All_standings[All_standings$Year>=yearmin & All_standings$Year<=yearmax,]$spread)
        MinSpread<-min(All_standings[All_standings$Year>=yearmin & All_standings$Year<=yearmax,]$spread)
        StartDate<-max(All_standings[All_standings$Year>=yearmin & All_standings$Year<=yearmax,]$date)
        EndDate<-min(All_standings[All_standings$Year>=yearmin & All_standings$Year<=yearmax,]$date)
        plot_AL_West<-All_standings[All_standings$Division=="AL West" & All_standings$Year>=yearmin & All_standings$Year<=yearmax,]
        
        team_division_colors<-sqldf("SELECT distinct Current_Name, HexColor, Max(W) as MaxW from plot_AL_West group by Current_Name")
        team_division_colors<-team_division_colors[order(-team_division_colors$MaxW),]
        
        grph_plot_title <- "AL West"
        
        par(mar = c(5, 5, 5, 15), xpd=TRUE )
        plot(plot_AL_West$date, plot_AL_West$spread
             , main= grph_plot_title, cex.main = 2.5
             , pch = 16
             , col=plot_AL_West$HexColor
             , cex.lab = 1.7, xlab="", ylab="Net Wins"
             , plot.window(xlim=c(EndDate,StartDate), ylim=c(MinSpread,MaxSpread), xaxs="i", yaxs="i") #
             #, cex.axis	= 1.1
             #    , log="x"
        )
        #grid(nx = NULL, ny = NULL, col = "lightgray", lty = 1,lwd = par("lwd"), equilogs = FALSE)
        legend(StartDate, MaxSpread
               #grph_min_date, 40
               , legend = team_division_colors[,"Current_Name"]
               , col=team_division_colors[,"HexColor"]
               , lty=1, lwd=8, cex=1.2
               ,box.lty=1)
    })
    
    output$NL_East_Graph<- renderPlot({
        yearmin<-as.numeric(input$max_year)-as.numeric(input$number_of_years)+1
        yearmax<-as.numeric(input$max_year)
        
        MaxSpread<-max(All_standings[All_standings$Year>=yearmin & All_standings$Year<=yearmax,]$spread)
        MinSpread<-min(All_standings[All_standings$Year>=yearmin & All_standings$Year<=yearmax,]$spread)
        StartDate<-max(All_standings[All_standings$Year>=yearmin & All_standings$Year<=yearmax,]$date)
        EndDate<-min(All_standings[All_standings$Year>=yearmin & All_standings$Year<=yearmax,]$date)
        
        plot_NL_East<-All_standings[All_standings$Division=="NL East" & All_standings$Year>=yearmin & All_standings$Year<=yearmax,]
        
        team_NL_East_colors<-sqldf("SELECT distinct Current_Name, HexColor, Max(W) as MaxW from plot_NL_East group by Current_Name")
        team_NL_East_colors<-team_NL_East_colors[order(-team_NL_East_colors$MaxW),]
        
        grph_plot_title <- "NL East"
        
        par(mar = c(5, 5, 5, 15), xpd=TRUE )
        plot(plot_NL_East$date, plot_NL_East$spread
             , main= grph_plot_title, cex.main = 2.5
             , pch = 16
             , col=plot_NL_East$HexColor
             , cex.lab = 1.7, xlab="", ylab="Net Wins"
             , plot.window(xlim=c(EndDate,StartDate), ylim=c(MinSpread,MaxSpread), xaxs="i", yaxs="i") #
             #, cex.axis	= 1.1
             #    , log="x"
        )
        #grid(nx = NULL, ny = NULL, col = "lightgray", lty = 1,lwd = par("lwd"), equilogs = FALSE)
        legend(StartDate, MaxSpread
               #grph_min_date, 40
               , legend = team_NL_East_colors[,"Current_Name"]
               , col=team_NL_East_colors[,"HexColor"]
               , lty=1, lwd=8, cex=1.2
               ,box.lty=1)
        
    })
    
    
    output$NL_Central_Graph<- renderPlot({
        yearmin<-as.numeric(input$max_year)-as.numeric(input$number_of_years)+1
        yearmax<-as.numeric(input$max_year)
        
        MaxSpread<-max(All_standings[All_standings$Year>=yearmin & All_standings$Year<=yearmax,]$spread)
        MinSpread<-min(All_standings[All_standings$Year>=yearmin & All_standings$Year<=yearmax,]$spread)
        StartDate<-max(All_standings[All_standings$Year>=yearmin & All_standings$Year<=yearmax,]$date)
        EndDate<-min(All_standings[All_standings$Year>=yearmin & All_standings$Year<=yearmax,]$date)
        
        
        plot_NL_Central<-All_standings[All_standings$Division=="NL Central" & All_standings$Year>=yearmin & All_standings$Year<=yearmax,]
        
        team_division_colors<-sqldf("SELECT distinct Current_Name, HexColor, Max(W) as MaxW from plot_NL_Central group by Current_Name")
        team_division_colors<-team_division_colors[order(-team_division_colors$MaxW),]
        
        grph_plot_title <- "NL Central"
        
        par(mar = c(5, 5, 5, 15), xpd=TRUE )
        plot(plot_NL_Central$date, plot_NL_Central$spread
             , main= grph_plot_title, cex.main = 2.5
             , pch = 16
             , col=plot_NL_Central$HexColor
             , cex.lab = 1.7, xlab="", ylab="Net Wins"
             , plot.window(xlim=c(EndDate,StartDate), ylim=c(MinSpread,MaxSpread), xaxs="i", yaxs="i") #
             #, cex.axis	= 1.1
             #    , log="x"
        )
        #grid(nx = NULL, ny = NULL, col = "lightgray", lty = 1,lwd = par("lwd"), equilogs = FALSE)
        legend(StartDate, MaxSpread
               #grph_min_date, 40
               , legend = team_division_colors[,"Current_Name"]
               , col=team_division_colors[,"HexColor"]
               , lty=1, lwd=8, cex=1.2
               ,box.lty=1)
    })
    
    
    output$NL_West_Graph<- renderPlot({
        yearmin<-as.numeric(input$max_year)-as.numeric(input$number_of_years)+1
        yearmax<-as.numeric(input$max_year)
        
        MaxSpread<-max(All_standings[All_standings$Year>=yearmin & All_standings$Year<=yearmax,]$spread)
        MinSpread<-min(All_standings[All_standings$Year>=yearmin & All_standings$Year<=yearmax,]$spread)
        StartDate<-max(All_standings[All_standings$Year>=yearmin & All_standings$Year<=yearmax,]$date)
        EndDate<-min(All_standings[All_standings$Year>=yearmin & All_standings$Year<=yearmax,]$date)
        
        
        plot_NL_West<-All_standings[All_standings$Division=="NL West" & All_standings$Year>=yearmin & All_standings$Year<=yearmax,]
        
        team_division_colors<-sqldf("SELECT distinct Current_Name, HexColor, Max(W) as MaxW from plot_NL_West group by Current_Name")
        team_division_colors<-team_division_colors[order(-team_division_colors$MaxW),]
        
        grph_plot_title <- "NL West"
        
        par(mar = c(5, 5, 5, 15), xpd=TRUE )
        plot(plot_NL_West$date, plot_NL_West$spread
             , main= grph_plot_title, cex.main = 2.5
             , pch = 16
             , col=plot_NL_West$HexColor
             , cex.lab = 1.7, xlab="", ylab="Net Wins"
             , plot.window(xlim=c(EndDate,StartDate), ylim=c(MinSpread,MaxSpread), xaxs="i", yaxs="i") #
             #, cex.axis	= 1.1
             #    , log="x"
        )
        #grid(nx = NULL, ny = NULL, col = "lightgray", lty = 1,lwd = par("lwd"), equilogs = FALSE)
        legend(StartDate, MaxSpread
               #"topright", inset=c(-0.18, .15)
               #grph_min_date, 40
               , legend = team_division_colors[,"Current_Name"]
               , col=team_division_colors[,"HexColor"]
               , lty=1, lwd=8, cex=1.2
               ,box.lty=1)
    })
    
}