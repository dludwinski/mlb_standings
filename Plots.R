
#Plots


year <- 2006
Division <- "AL East"

plot_temp<-All_standings[All_standings$Division==Division & All_standings$Year>2022,]

team_division_colors<-sqldf("SELECT distinct Tm, HexColor, Max(W) as MaxW from plot_temp group by Tm")
team_division_colors<-team_division_colors[order(-team_division_colors$MaxW),]

grph_plot_title <- paste0("Net wins by date: ",Division ,", ",year)

par(mar = c(5, 5, 5, 10), xpd=TRUE )
plot(plot_temp$date, plot_temp$spread
     , main= grph_plot_title, cex.main = 2.5
     , pch = 16
     , col=plot_temp$HexColor
     , cex.lab = 1.7, xlab="Date", ylab="Net Wins"
     #, plot.window(xlim=c(season_start,season_end), ylim=c(-80,80), xaxs="i", yaxs="i") #
     #, cex.axis	= 1.1
     #    , log="x"
)
#grid(nx = NULL, ny = NULL, col = "lightgray", lty = 1,lwd = par("lwd"), equilogs = FALSE)
legend("topright", inset=c(-0.10, .15)
       #grph_min_date, 40
       , legend = team_division_colors[,"Tm"]
       , col=team_division_colors[,"HexColor"]
       , lty=1, lwd=8, cex=1.3
       ,box.lty=1)



points(plot_temp$date, plot_temp$spread
       ,pch = 16
       ,col=plot_temp$HexColor
) 

ggplot(All_standings[All_standings$Division==Division & All_standings$Year==year,], aes(date, spread, group = Tm, colour = Tm)) + 
    geom_point() + geom_line(size = 1) + theme_bw() + ylab("Wins-Losses") +
    scale_colour_manual(name = "Team", 
                        values = c("green3", "orange", "blue", "red", "grey")) + 
    ggtitle(paste0("Net wins by date: ",Division ,", ",year))

