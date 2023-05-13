#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
#library(ggplot2)


fluidPage(
    fluidRow(
        column(12,
            titlePanel("Net wins (Wins - Losses) over the season"),
            
            selectInput("max_year", "End Year", 
                        choices = c("2023", "2022", "2021", "2020", "2019", "2018", "2017", "2016", "2015", "2014", "2013", "2012", "2011", "2010", "2009", "2008", "2007", "2006", "2005", "2004", "2003", "2002", "2001", "2000", "1999", "1998", "1997", "1996", "1995", "1994", "1993", "1992", "1991", "1990") ),
            selectInput("number_of_years", "Number of Years", 
                        choices = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "15", "20", "25", "30") ),
        ),
        
        column(12,
               
            plotOutput(
                "AL_East_Graph",
                width = "100%",
                height = "400px",
                click = NULL,
                dblclick = NULL,
                hover = NULL,
                brush = NULL,
                inline = FALSE
            ),
               
            plotOutput(
                "AL_Central_Graph",
                width = "100%",
                height = "400px",
                click = NULL,
                dblclick = NULL,
                hover = NULL,
                brush = NULL,
                inline = FALSE
            ),
            plotOutput(
                "AL_West_Graph",
                width = "100%",
                height = "400px",
                click = NULL,
                dblclick = NULL,
                hover = NULL,
                brush = NULL,
                inline = FALSE
            ),
        
            plotOutput(
                "NL_East_Graph",
                width = "100%",
                height = "400px",
                click = NULL,
                dblclick = NULL,
                hover = NULL,
                brush = NULL,
                inline = FALSE
            ),
            
            plotOutput(
                "NL_Central_Graph",
                width = "100%",
                height = "400px",
                click = NULL,
                dblclick = NULL,
                hover = NULL,
                brush = NULL,
                inline = FALSE
            ),
            plotOutput(
                "NL_West_Graph",
                width = "100%",
                height = "400px",
                click = NULL,
                dblclick = NULL,
                hover = NULL,
                brush = NULL,
                inline = FALSE
            )
        ),
        column(1,
            
        )
    )
)