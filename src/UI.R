library(shiny)
library(leaflet)
library(ggplot2)
library(fishualize)
library(dplyr)

# Define UI for application
ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"), # Link to the external CSS file
    tags$script(src = "playMusic.js")  # Load the custom JavaScript file
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
          selectInput("species", "Species", choices = NULL),
          
          # Size/abundance input
          selectInput("indicator", "Indicator", choices = c("Abundance", "Size"), selected = "Abundance"),
          
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
  ),
  
  div(class = "switch-container",  # Toggle switch overlay with opaque white background
      tags$label("Sound:",
                 tags$input(type = "checkbox", id = "toggle_music", checked = TRUE)
      )
  ),
  
  div(id = "fish_puns",
      actionButton("fish_puns_btn", "Fish Pun")),
  
  # Placeholder for dynamic legend
  div(id = "legendContainer",
      class = "legend-container",
      h4(class = "legend-title", "Marker Size Legend"),
      uiOutput("legendSmall"),
      uiOutput("legendMedium"),
      uiOutput("legendLarge"),
      uiOutput("legendExtraLarge")
  )
)
