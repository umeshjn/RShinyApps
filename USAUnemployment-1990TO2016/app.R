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
library(tidyverse)
library(tidycensus)
library(shinythemes)

data <- read_csv("./data/UnemploymentRates1990-2016.csv")

# Define UI for application
ui <- navbarPage("Unemployment Rates in USA 1990 to 2016",
        tabPanel( "Interactive Maps for Exploration",
            fluidPage(

                # theme
                theme = shinytheme("flatly"),
        
                # Sidebar with required filter options
                sidebarLayout(
                    sidebarPanel(
                        helpText("Here in this application you can interactively explore the unemployment rates in every county of the selected state in USA for a particular period."),
                        
                        selectInput("state", "Select the State::",
                                    c("All" = "all",
                                      "Alabama" = "al",
                                      "Alaska" = "ak",
                                      "Arizona" = "az",
                                      "Arkansas" = "ar",
                                      "California" = "ca",
                                      "Colorado" = "co",
                                      "Connecticut" = "ct",
                                      "Delaware" = "de",
                                      "Florida" = "fl",
                                      "Georgia" = "ga",
                                      "Hawaii" = "hi",
                                      "Idado" = "id",
                                      "Illinois" = "il",
                                      "Indiana" = "in",
                                      "Iowa" = "ia",
                                      "Kansas" = "ks",
                                      "Kentucky" = "ky",
                                      "Louisiana" = "la",
                                      "Maine" = "me", 
                                      "Maryland" = "md",
                                      "Massachusetts" = "ma",
                                      "Michigan" = "mi",
                                      "Minnesota" = "mn",
                                      "Mississippi" = "ms",
                                      "Missouri" = "mo",
                                      "Montana" = "mt",
                                      "Nebraska" = "ne",
                                      "Nevada" = "nv",
                                      "New Hampshire" = "nh",
                                      "New Jersey" = "nj",
                                      "New Mexico" = "nm",
                                      "New York" = "ny",
                                      "North Carolina" = "nc",
                                      "North Dakota" = "nd",
                                      "Ohio" = "oh",
                                      "Oklahama" = "ok",
                                      "Oregon" = "or",
                                      "Pennsylvania" = "pa",
                                      "Rhode Island" ="ri",
                                      "South Carolina" = "sc",
                                      "South Dakota" = "sd",
                                      "Tennessee" = "tn",
                                      "Texas" = "tx",
                                      "Utah" = "ut",
                                      "Vermont" = "vt",
                                      "Virginia" = "va",
                                      "Washington" = "wa",
                                      "West Virginia" = "wv",
                                      "Wisconsin" = "wi",
                                      "Wyoming" = "wy")),
                        selectInput("year", "Select the Year::", sort(unique(data$Year))),
                        selectInput("month", "Select the Month::", month.name),
                        sliderInput("raterange", label = "Select the Rate Range", min = 0, 
                                    max = max(data$Rate), value = c(0, max(data$Rate))),
                        selectInput("color", "Select the Color Palette ::",
                                    c("Viridis" = "viridis", 
                                      "Cividis" = "cividis",
                                      "Inferno" = "inferno",
                                      "Magma" = "magma",
                                      "Plasma" = "plasma"),
                                    selected = "magma")
                    ),
        
                # Show the map
                mainPanel(
                    highchartOutput("plot", width = "700px", height = "500px")
                )
            )
        )
    )
)

# Define server logic
server <- function(input, output) {

    output$plot <- renderHighchart({
        color = case_when(
            input$color == "viridis" ~ viridis(10),
            input$color == "cividis" ~ cividis(10),
            input$color == "inferno" ~ inferno(10),
            input$color == "magma" ~ magma(10),
            input$color == "plasma" ~ plasma(10)
        )
        if (input$state == "all") {
            hcmap("countries/us/us-all-all", data = data %>% subset(Year == input$year & Month == input$month & Rate > input$raterange[1] & Rate < input$raterange[2]),
                  name = "Unemployment Rate", value = "Rate", joinBy = "fips",
                  borderColor = "transparent") %>%
                hc_colorAxis(stops = color_stops(10, colors = color)) %>% 
                hc_legend(layout = "vertical", align = "left",
                          verticalAlign = "bottom",
                          floating = TRUE, valueDecimals = 0, valueSuffix = "%") %>%
                hc_title(text = paste("Unemployment Rate in USA ", "-",  input$year, input$month, sep = " "))
        } else {
            hcmap(paste("countries/us/us-",input$state, "-all", sep = ""), data = data %>% subset(Year == input$year & Month == input$month & Rate > input$raterange[1] & Rate < input$raterange[2]),
              name = "Unemployment Rate", value = "Rate", joinBy = "fips",
              borderColor = "transparent") %>%
            hc_colorAxis(stops = color_stops(10, colors = color)) %>% 
            hc_legend(layout = "vertical", align = "left",
                      verticalAlign = "bottom",
                      floating = TRUE, valueDecimals = 0, valueSuffix = "%") %>%
            hc_title(text = paste("Unemployment Rate in the State of ", toupper(input$state), "-",  input$year, input$month, sep = " "))
        }    
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
