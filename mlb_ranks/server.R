library(shiny)
library(sqldf)
library(ggplot2)

All_standings <- readRDS(url("https://github.com/dludwinski/mlb_standings/blob/main/All_standings.RDS?raw=true"))


function(input, output) {
    output$NL_East_Graph<- renderPlot({
        Division <- "NL East"
        year <- input$sel_year
        yearmin<-as.numeric(input$max_year)-as.numeric(input$number_of_years)+1
        yearmax<-as.numeric(input$max_year)
        
        plot_temp<-All_standings[All_standings$Division==Division & All_standings$Year>=yearmin & All_standings$Year<=yearmax ,]
        
        team_division_colors<-sqldf("SELECT Current_Name, HexColor, Max(W) as MaxW from plot_temp group by Current_Name")
        team_division_colors<-team_division_colors[order(-team_division_colors$MaxW),]
        
        grph_plot_title <- Division
        
        plot_temp$Current_Name <- factor(plot_temp$Tm, levels = team_division_colors[,"Current_Name"])
        
        ggplot(plot_temp, aes(date, spread
                              , group = Current_Name, colour = Current_Name)) + 
            geom_point(size = 3) +  theme_bw() + ylab("Wins-Losses") + xlab("") +
            scale_colour_manual(name = "Team", 
                                values = team_division_colors$HexColor )+ 
            ggtitle(grph_plot_title) +
            theme(legend.key.size = unit(1.5, 'cm'),
                  legend.title = element_text(size=20),
                  legend.text = element_text(size=16),
                  plot.title = element_text(size=24),
                  axis.text=element_text(size=14),
                  axis.title=element_text(size=14,face="bold") )   +
            guides(color = guide_legend(override.aes = list(size=6)))
    })
    
    
    output$NL_Central_Graph<- renderPlot({
        yearmax<-as.numeric(input$max_year)
        if (yearmax>1993){
            Division <- "NL Central"
            year <- input$sel_year
            yearmin<-as.numeric(input$max_year)-as.numeric(input$number_of_years)+1
            
            plot_temp<-All_standings[All_standings$Division==Division & All_standings$Year>=yearmin & All_standings$Year<=yearmax ,]
            
            team_division_colors<-sqldf("SELECT Current_Name, HexColor, Max(W) as MaxW from plot_temp group by Current_Name")
            team_division_colors<-team_division_colors[order(-team_division_colors$MaxW),]
            
            grph_plot_title <- Division
            
            plot_temp$Current_Name <- factor(plot_temp$Tm, levels = team_division_colors[,"Current_Name"])
            
            ggplot(plot_temp, aes(date, spread
                                  , group = Current_Name, colour = Current_Name)) + 
                geom_point(size = 3) +  theme_bw() + ylab("Wins-Losses") + xlab("") +
                scale_colour_manual(name = "Team", 
                                    values = team_division_colors$HexColor )+ 
                ggtitle(grph_plot_title) +
                theme(legend.key.size = unit(1.5, 'cm'),
                      legend.title = element_text(size=20),
                      legend.text = element_text(size=16),
                      plot.title = element_text(size=24),
                      axis.text=element_text(size=14),
                      axis.title=element_text(size=14,face="bold") )   +
                guides(color = guide_legend(override.aes = list(size=6)))
        }
        else{  return(NULL)}
    })
    
    
    output$NL_West_Graph<- renderPlot({
        Division <- "NL West"
        year <- input$sel_year
        yearmin<-as.numeric(input$max_year)-as.numeric(input$number_of_years)+1
        yearmax<-as.numeric(input$max_year)
        
        plot_temp<-All_standings[All_standings$Division==Division & All_standings$Year>=yearmin & All_standings$Year<=yearmax ,]
        
        team_division_colors<-sqldf("SELECT Current_Name, HexColor, Max(W) as MaxW from plot_temp group by Current_Name")
        team_division_colors<-team_division_colors[order(-team_division_colors$MaxW),]
        
        grph_plot_title <- Division
        
        plot_temp$Current_Name <- factor(plot_temp$Tm, levels = team_division_colors[,"Current_Name"])
        
        ggplot(plot_temp, aes(date, spread
                              , group = Current_Name, colour = Current_Name)) + 
            geom_point(size = 3) +  theme_bw() + ylab("Wins-Losses") + xlab("") +
            scale_colour_manual(name = "Team", 
                                values = team_division_colors$HexColor )+ 
            ggtitle(grph_plot_title) +
            theme(legend.key.size = unit(1.5, 'cm'),
                  legend.title = element_text(size=20),
                  legend.text = element_text(size=16),
                  plot.title = element_text(size=24),
                  axis.text=element_text(size=14),
                  axis.title=element_text(size=14,face="bold") )   +
            guides(color = guide_legend(override.aes = list(size=6)))
    })
    
    
    output$AL_East_Graph<- renderPlot({
        Division <- "AL East"
        year <- input$sel_year
        yearmin<-as.numeric(input$max_year)-as.numeric(input$number_of_years)+1
        yearmax<-as.numeric(input$max_year)
        
        plot_temp<-All_standings[All_standings$Division==Division & All_standings$Year>=yearmin & All_standings$Year<=yearmax ,]
        
        team_division_colors<-sqldf("SELECT Current_Name, HexColor, Max(W) as MaxW from plot_temp group by Current_Name")
        team_division_colors<-team_division_colors[order(-team_division_colors$MaxW),]
        
        grph_plot_title <- Division
        
        plot_temp$Current_Name <- factor(plot_temp$Tm, levels = team_division_colors[,"Current_Name"])
        
        ggplot(plot_temp, aes(date, spread
                              , group = Current_Name, colour = Current_Name)) + 
            geom_point(size = 3) +  theme_bw() + ylab("Wins-Losses") + xlab("") +
            scale_colour_manual(name = "Team", 
                                values = team_division_colors$HexColor )+ 
            ggtitle(grph_plot_title) +
            theme(legend.key.size = unit(1.5, 'cm'),
                  legend.title = element_text(size=20),
                  legend.text = element_text(size=16),
                  plot.title = element_text(size=24),
                  axis.text=element_text(size=14),
                  axis.title=element_text(size=14,face="bold") )   +
        guides(color = guide_legend(override.aes = list(size=6)))
    })
    
    output$AL_Central_Graph<- renderPlot({
        Division <- "AL Central"
        yearmax<-as.numeric(input$max_year)
        if (yearmax>1993){
            yearmin<-as.numeric(input$max_year)-as.numeric(input$number_of_years)+1
            All_standings_yr<-All_standings[All_standings$Division==Division & All_standings$Year>=yearmin & All_standings$Year<=yearmax ,]
            plot_temp<-All_standings[All_standings$Division==Division & All_standings$Year>=yearmin & All_standings$Year<=yearmax ,]
            
            team_division_colors<-sqldf("SELECT Current_Name, HexColor, Max(W) as MaxW from plot_temp group by Current_Name")
            team_division_colors<-team_division_colors[order(-team_division_colors$MaxW),]
            
            grph_plot_title <- Division
            
            plot_temp$Current_Name <- factor(plot_temp$Tm, levels = team_division_colors[,"Current_Name"])
            
            ggplot(plot_temp, aes(date, spread
                                  , group = Current_Name, colour = Current_Name)) + 
                geom_point(size = 3) +  theme_bw() + ylab("Wins-Losses") + xlab("") +
                scale_colour_manual(name = "Team", 
                                    values = team_division_colors$HexColor )+ 
                ggtitle(grph_plot_title) +
                theme(legend.key.size = unit(1.5, 'cm'),
                      legend.title = element_text(size=20),
                      legend.text = element_text(size=16),
                      plot.title = element_text(size=24),
                      axis.text=element_text(size=14),
                      axis.title=element_text(size=14,face="bold") )   +
                guides(color = guide_legend(override.aes = list(size=6)))
        }
    })
    
    
    output$AL_West_Graph<- renderPlot({
        Division <- "AL West"
        year <- input$sel_year
        yearmin<-as.numeric(input$max_year)-as.numeric(input$number_of_years)+1
        yearmax<-as.numeric(input$max_year)
        
        plot_temp<-All_standings[All_standings$Division==Division & All_standings$Year>=yearmin & All_standings$Year<=yearmax ,]
        
        team_division_colors<-sqldf("SELECT Current_Name, HexColor, Max(W) as MaxW from plot_temp group by Current_Name")
        team_division_colors<-team_division_colors[order(-team_division_colors$MaxW),]
        
        grph_plot_title <- Division
        
        plot_temp$Current_Name <- factor(plot_temp$Tm, levels = team_division_colors[,"Current_Name"])
        
        ggplot(plot_temp, aes(date, spread
                              , group = Current_Name, colour = Current_Name)) + 
            geom_point(size = 3) +  theme_bw() + ylab("Wins-Losses") + xlab("") +
            scale_colour_manual(name = "Team", 
                                values = team_division_colors$HexColor )+ 
            ggtitle(grph_plot_title) +
            theme(legend.key.size = unit(1.5, 'cm'),
                  legend.title = element_text(size=20),
                  legend.text = element_text(size=16),
                  plot.title = element_text(size=24),
                  axis.text=element_text(size=14),
                  axis.title=element_text(size=14,face="bold") )   +
            guides(color = guide_legend(override.aes = list(size=6)))
    })
    
       
}