library(shiny)
library(sqldf)
library(ggplot2)
library(dplyr)

#Read only data
All_standings <- readRDS(url("https://github.com/dludwinski/mlb_standings/blob/main/All_standings.RDS?raw=true"))



function(input, output, session) {
    #Reactive
    plot_temp <- reactive({
        year <- input$sel_year
        yearmax<-as.numeric(input$max_year)
        yearmin<-as.numeric(input$max_year)-as.numeric(input$number_of_years)+1
        plot_temp2<-All_standings[All_standings$Year>=yearmin & All_standings$Year<=yearmax ,]
        
        return(plot_temp2)
        
    })
    max_spread <- reactive({
        max_s1 <- max(plot_temp()$spread)
        return(max_s1)
        
    })
    min_spread <- reactive({
        min_s1 <- min(plot_temp()$spread)
        return(min_s1)
        
    })
    output$NL_East_Graph<- renderPlot({
        plot_temp_graph<-dplyr::filter(plot_temp(), Division=="NL East")
        grph_plot_title <- "NL East"
        
        team_division_colors<-sqldf("SELECT Tm, HexColor, Max(W) as MaxW from plot_temp_graph group by Tm")
        team_division_colors<-team_division_colors[order(-team_division_colors$MaxW),]
        
        plot_temp_graph$Tm <- factor(plot_temp_graph$Tm, levels = team_division_colors[,"Tm"])
        
        ggplot(plot_temp_graph, aes(date, spread
                                  , group = Tm, colour = Tm)) + 
            geom_point(size = 3) +  theme_bw() + ylab("Wins-Losses") + xlab("") +
            ylim(min_spread(), max_spread()) +
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
            plot_temp_graph<-dplyr::filter(plot_temp(), Division=="NL Central")
            grph_plot_title <- "NL Central"
            
            team_division_colors<-sqldf("SELECT Tm, HexColor, Max(W) as MaxW from plot_temp_graph group by Tm")
            team_division_colors<-team_division_colors[order(-team_division_colors$MaxW),]
            
            plot_temp_graph$Tm <- factor(plot_temp_graph$Tm, levels = team_division_colors[,"Tm"])
            
            ggplot(plot_temp_graph, aes(date, spread
                                        , group = Tm, colour = Tm)) + 
                geom_point(size = 3) +  theme_bw() + ylab("Wins-Losses") + xlab("") +
                ylim(min_spread(), max_spread()) +
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
        plot_temp_graph<-dplyr::filter(plot_temp(), Division=="NL West")
        grph_plot_title <- "NL West"
        
        team_division_colors<-sqldf("SELECT Tm, HexColor, Max(W) as MaxW from plot_temp_graph group by Tm")
        team_division_colors<-team_division_colors[order(-team_division_colors$MaxW),]
        
        plot_temp_graph$Tm <- factor(plot_temp_graph$Tm, levels = team_division_colors[,"Tm"])
        
        ggplot(plot_temp_graph, aes(date, spread
                                    , group = Tm, colour = Tm)) + 
            geom_point(size = 3) +  theme_bw() + ylab("Wins-Losses") + xlab("") +
            ylim(min_spread(), max_spread()) +
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
        plot_temp_graph<-dplyr::filter(plot_temp(), Division=="AL East")
        grph_plot_title <- "AL East"
        
        team_division_colors<-sqldf("SELECT Tm, HexColor, Max(W) as MaxW from plot_temp_graph group by Tm")
        team_division_colors<-team_division_colors[order(-team_division_colors$MaxW),]
        
        plot_temp_graph$Tm <- factor(plot_temp_graph$Tm, levels = team_division_colors[,"Tm"])
        
        ggplot(plot_temp_graph, aes(date, spread
                                    , group = Tm, colour = Tm)) + 
            geom_point(size = 3) +  theme_bw() + ylab("Wins-Losses") + xlab("") +
            ylim(min_spread(), max_spread()) +
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
        yearmax<-as.numeric(input$max_year)
        if (yearmax>1993){
            plot_temp_graph<-dplyr::filter(plot_temp(), Division=="AL Central")
            grph_plot_title <- "AL Central"
            
            team_division_colors<-sqldf("SELECT Tm, HexColor, Max(W) as MaxW from plot_temp_graph group by Tm")
            team_division_colors<-team_division_colors[order(-team_division_colors$MaxW),]
            
            plot_temp_graph$Tm <- factor(plot_temp_graph$Tm, levels = team_division_colors[,"Tm"])
            
            ggplot(plot_temp_graph, aes(date, spread
                                        , group = Tm, colour = Tm)) + 
                geom_point(size = 3) +  theme_bw() + ylab("Wins-Losses") + xlab("") +
                ylim(min_spread(), max_spread()) +
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
        plot_temp_graph<-dplyr::filter(plot_temp(), Division=="AL West")
        grph_plot_title <- "AL West"
        
        team_division_colors<-sqldf("SELECT Tm, HexColor, Max(W) as MaxW from plot_temp_graph group by Tm")
        team_division_colors<-team_division_colors[order(-team_division_colors$MaxW),]
        
        plot_temp_graph$Tm <- factor(plot_temp_graph$Tm, levels = team_division_colors[,"Tm"])
        
        ggplot(plot_temp_graph, aes(date, spread
                                    , group = Tm, colour = Tm)) + 
            geom_point(size = 3) +  theme_bw() + ylab("Wins-Losses") + xlab("") +
            ylim(min_spread(), max_spread()) +
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