library(shiny)
library(leaflet)
library(ggplot2)
library(fishualize)

# Define UI for application
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
            html, body {
                width: 100%;
                height: 100%;
                margin: 0;
                padding: 0;
                overflow: hidden;
            }
            #mapContainer {
                position: relative;
                width: 100%;
                height: 100vh;
            }
            #sidebar {
                position: absolute;
                top: 0;
                right: 0;
                width: 300px;
                height: 100%;
                background-color: rgba(255, 255, 255, 0.9); /* semi-transparent white background */
                box-shadow: -5px 0 15px rgba(0, 0, 0, 0.2); /* optional: add shadow */
                padding: 20px;
                overflow-y: auto;
            }
        "))
  ),
  div(id = "mapContainer",
      leafletOutput("swedenMap", width = "100%", height = "100%"),  # Map container
      div(id = "sidebar",  # Sidebar for controls
          h4("Map Controls"),
          
          # Marker input
          radioButtons("marker", "Marker:",
                       choices = c("Circles", "Fish"),
                       selected = "Circles",
                       inline = TRUE),
          
          # Species input
          selectInput("species", "Species", choices = c("Red", "Blue", "Green")),
          
          # Size/abundance input
          selectInput("size", "Size", choices = c("Small", "Medium", "Large")),
          
          # Year input
          sliderInput("year", "Year:",
                      min = 2000, max = 2020,
                      value = 2015, sep = ""),
          br(),
          
          # Choose plot type
          radioButtons("plotType", "Choose Plot Type:",
                       choices = c("Mean", "Median"),
                       selected = "Mean", inline = TRUE),
          br(),
          
          # Display plot
          plotOutput("barplot", height = "200px")
      )
  )
)