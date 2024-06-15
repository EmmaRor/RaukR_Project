# Example data for bar plot
df <- data.frame(
  category = c("A", "B", "C", "D"),
  value = c(3, 5, 4, 6),
  stddev = c(0.5, 0.7, 0.6, 0.8)
)

# Sample data with latitude and longitude
marker_data <- data.frame(
  name = c("Location A", "Location B", "Location C"),
  lat = c(59.3293, 57.7089, 55.6049),
  lng = c(18.0686, 11.9746, 13.0038),
  value = c(30, 5, 10),
  category = c("A", "B", "C")
)

# Define server logic
server <- function(input, output, session) {
  
  # The map
  output$swedenMap <- renderLeaflet({
    leaflet() %>%                                     # Create a leaflet map object
      addTiles() %>%                                  # Default OpenStreetMap tiles
      setView(lng = 18.6435, lat = 60.1282, zoom = 5) # Somewhere in Sweden
  })
  
  # The marker (fish vs circles)
  observeEvent(input$marker, {

    if (input$marker == "Circles") {
      leafletProxy("swedenMap", data = marker_data) %>%
        clearMarkers() %>%                    # Clear any existing markers
        addCircleMarkers( 
          lng = ~lng, lat = ~lat,             # Set longitude and latitude
          label = ~name,                      # Label each marker
          stroke = FALSE, fillOpacity = 0.5,  # Border and fill opacity
          popup = ~paste0("<b>", name, "</b><br>Value: ", value, "<br>Category: ", category)
        )
    } 
    
    else if (input$marker == "Fish") {
      leafletProxy("swedenMap", data = marker_data) %>%
        clearMarkers() %>%           # Clear any existing markers
        addMarkers(                  # Add custom icon markers
          lng = ~lng, lat = ~lat,    # Set longitude and latitude
          icon = ~icons(             # Define custom icon properties
            iconUrl = "https://as1.ftcdn.net/v2/jpg/06/70/58/68/1000_F_670586814_zPsLZ38T5wtVC4vDKIGNHCN8aYXPSqo1.webp",
            iconWidth = ~value * 5, 
            iconHeight = ~value * 5,
            iconAnchorX = ~value * 2.5,
            iconAnchorY = ~value * 2.5,
            popupAnchorX = 0,
            popupAnchorY = -value * 2.5
          ),
          label = ~name,             # Label each marker
          popup = ~paste0("<b>", name, "</b><br>Value: ", value, "<br>Category: ", category)  
        )
    }
  })
  
  # Reactive expression for mean calculation
  meanData <- reactive({
    with(df, tapply(value, category, mean))
  })
  
  # Reactive expression for median calculation
  medianData <- reactive({
    with(df, tapply(value, category, median))
  })
  
  # Render the bar plot (mean vs median)
  output$barplot <- renderPlot({

    if (input$plotType == "Mean") {
      ggplot(df, aes(x = category, y = value)) +
        geom_bar(stat = "identity", fill = "blue", color = "black") +
        geom_errorbar(aes(ymin = value - stddev, ymax = value + stddev), width = 0.4,
                      position = position_dodge(0.9)) +
        labs(title = "Bar Plot of Means with Error Bars", 
             x = "Category", y = "Value") 
    } 

    else if (input$plotType == "Median") {
      ggplot(df, aes(x = category, y = value)) +
        geom_bar(stat = "identity", fill = "blue", color = "black") +
        labs(title = "Bar Plot of Medians without Error Bars", 
             x = "Category", y = "Value") 
    }
  })
}