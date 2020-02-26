#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(highcharter)
library(tidycensus)
library(tidyverse)
library(shinythemes)
library(readxl)
library(readr)

data1 <- read_excel("./data/AustralianPopulation.xlsx")

# Define UI for application
ui <- navbarPage( "Australian Population",
            tabPanel("Interactive Map",
            fluidPage(
        
            # Selecting the theme
            theme = shinytheme("flatly"),
        
            # Sidebar with options to filter the views
            sidebarLayout(
                sidebarPanel(
                    helpText("Australia is the largest country in Oceania and the world's sixth-largest country by total area"),
                    helpText("This application lets you explore the population of the continent among different age groups. This data is from Australian Bureau of Satistics."),
                    selectInput("age",
                              "Select the Age Group or Total:",
                               c("0 to 4" = "0 to 4",
                                 "5 to 9" = "5 to 9",
                                 "10 to 14" = "10 to 14",
                                 "15 to 19" = "15 to 19",
                                 "20 to 24" = "20 to 24",
                                 "25 to 29" = "25 to 29",
                                 "30 to 34" = "30 to 34",
                                 "35 to 39" = "35 to 39",
                                 "40 to 44" = "40 to 44",
                                 "45 to 49" = "45 to 49",
                                 "50 to 54" = "50 to 54",
                                 "55 to 59" = "55 to 59",
                                 "60 to 64" = "60 to 64",
                                 "65 to 69" = "65 to 69",
                                 "70 to 74" = "70 to 74",
                                 "75 to 79" = "75 to 79",
                                 "80 to 84" = "80 to 84",
                                 "85 and over" = "85 and over",
                                 "Total Persons" = "Total Persons")),
                    selectInput("color", "Select Color Palette::",
                                c("Magma" ="magma",
                                  "Plasma" = "plasma",
                                  "Inferno" = "inferno",
                                  "Viridis" = "viridis",
                                  "Cividis" = "cividis"))
                ),
        
                # Show the map
                mainPanel(
                   highchartOutput("map", width = "700px", height = "500px")
                )
            )
        )
    )
)

# Define server logic 
server <- function(input, output) {

    output$map <- renderHighchart({
        color = case_when(
            input$color == "viridis" ~ viridis(10),
            input$color == "cividis" ~ cividis(10),
            input$color == "inferno" ~ inferno(10),
            input$color == "magma" ~ magma(10),
            input$color == "plasma" ~ plasma(10)
        )
        print(data1 %>% filter(AgeGroup == input$age))
        hcmap("countries/au/au-all", data = data1 %>% filter(AgeGroup == input$age), 
              name = "Population", value = "TotalPopulation", joinBy = c("woe-name", "State"),
              borderColor = "transparent",
              dataLabels = list(enabled = TRUE, format = '{point.name}')) %>% 
            hc_legend(layout = "horizontal", align = "left",
                      floating = TRUE, valueDecimals = 0) %>%
            hc_colorAxis(stops = color_stops(10, colors = color)) %>%
            hc_credits(enabled = TRUE, text = "Australian Bureau of Statistics",
                       href = "https://www.abs.gov.au/AUSSTATS/abs@.nsf/DetailsPage/3235.02018?OpenDocument")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
