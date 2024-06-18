# # Example data for bar plot
# df <- data.frame(
#   category = c("A", "B", "C", "D"),
#   value = c(3, 5, 4, 6),
#   stddev = c(0.5, 0.7, 0.6, 0.8)
# )
# 
# # Sample data with latitude and longitude
# marker_data <- data.frame(
#   name = c("Location A", "Location B", "Location C"),
#   lat = c(59.3293, 57.7089, 55.6049),
#   lng = c(18.0686, 11.9746, 13.0038),
#   value = c(30, 5, 10),
#   category = c("A", "B", "C")
# )

# Define server logic
server <- function(input, output, session) {
  
  # Read data file
  data <- read.csv("../Catch_20232019.csv")
  fish_puns <- readLines("../data/fish_puns.txt")
  
  # These things are only done once
  species <- unique(data$Species)
  species <- species[order(species)]
  updateSelectInput(session, "species", choices = species, selected = "Abborre")
  
  max_year <- max(data$Year)
  min_year <- min(data$Year)
  middle_year <- round((max_year + min_year) / 2, 0)
  updateSliderInput(session, "year",
                    min = min_year,
                    max = max_year,
                    value = middle_year)
  
  
  # Update dataframe based on choices
  reactive_data <- reactive({
    data[which(data$Species == input$species & data$Year == input$year), ]
  })
  
  reactive_marker_data <- reactive({
    df <- reactive_data()
    
    if (is.null(df) || nrow(df) == 0) {
      return(NULL)
    }
    
    display_df <- df %>%
      group_by(Location, Latitude, Longitude) %>%
      count()
    
    display_df_sizes <- display_df %>%
      mutate(size_category = case_when(
        n > (0.75 * max(display_df$n)) ~ 20,
        n > (0.5 * max(display_df$n)) ~ 15,
        n > (0.25 * max(display_df$n)) ~ 10,
        TRUE ~ 5
      ))
    
    return(display_df_sizes)
  })

  
  # The map
  output$swedenMap <- renderLeaflet({
    leaflet() %>%                                     # Create a leaflet map object
      addTiles() %>%                                  # Default OpenStreetMap tiles
      setView(lng = 18.6435, lat = 60.1282, zoom = 5) # Somewhere in Sweden
  })
  
  # The marker (fish vs circles)
  # The marker (fish vs circles)
  observe({
    df <- reactive_marker_data()
    marker <- input$marker
    
    if (is.null(df) || nrow(df) == 0) {
      return(NULL)
    }
    
    if (marker == "Circles") {
      leafletProxy("swedenMap", data = df) %>%
        clearMarkers() %>%
        addCircleMarkers(
          lng = ~Longitude, lat = ~Latitude,     # Set longitude and latitude
          label = ~Location,                     # Label each marker
          radius = ~size_category,               # Radius size
          stroke = FALSE, fillOpacity = 0.5,     # Border and fill opacity
          popup = ~paste0("<b>", Location, "</b><br>Amount: ", n)
        )
    } else if (marker == "Fish") {
      fish_multiplier = 3
      
      leafletProxy("swedenMap", data = df) %>%
        clearMarkers() %>%
        addMarkers(
          lng = ~Longitude, lat = ~Latitude,     # Set longitude and latitude
          icon = ~icons(
            iconUrl = "https://as1.ftcdn.net/v2/jpg/06/70/58/68/1000_F_670586814_zPsLZ38T5wtVC4vDKIGNHCN8aYXPSqo1.webp",
            iconWidth = ~size_category * fish_multiplier,      # Scale icon width
            iconHeight = ~size_category * fish_multiplier,     # Scale icon height
            iconAnchorX = ~size_category * fish_multiplier,
            iconAnchorY = ~size_category * fish_multiplier,
            popupAnchorX = 0,
            popupAnchorY = 0
          ),
          label = ~Location,                     # Label each marker
          popup = ~paste0("<b>", Location, "</b><br>Amount: ", n)
        )
    }
  })
  
  # observe({
  # 
  #   df <- reactive_data()
  #   marker <- input$marker
  #   
  #   if (nrow(df) == 0){
  #     return(NULL)
  #   }
  #   
  #   print(unique(df$Species))
  # 
  #   display_df <- df %>%
  #     group_by(Location, Latitude, Longitude) %>%
  #     count()
  #   
  #   display_df_sizes <- display_df %>%
  #     mutate(size_category = case_when(
  #       n > (0.75 * max(display_df$n)) ~ 4,
  #       n > (0.5 * max(display_df$n)) ~ 3,
  #       n > (0.25 * max(display_df$n)) ~ 2,
  #       TRUE ~ 1
  #     ))
  #   
  # 
  #   if (marker == "Circles") {
  #     
  #     print("Makring circles")
  #     
  #     leafletProxy("swedenMap", data = display_df_sizes %>%
  #       clearMarkers() %>%
  #       addCircleMarkers( 
  #         lng = ~Longitude, lat = ~Latitude,             # Set longitude and latitude
  #         label = ~Location,                      # Label each marker
  #         radius = ~size_category,
  #         stroke = FALSE, fillOpacity = 0.5,  # Border and fill opacity
  #         popup = ~paste0("<b>", Location, "</b><br>Amount: ", n)
  #       )
  #     )
  #   } 
  #   
  #   else if (marker == "Fish") {
  #     leafletProxy("swedenMap", data = display_df_sizes) %>%
  #       clearMarkers() %>%
  #       addMarkers(                  # Add custom icon markers
  #         lng = ~Longitude, lat = ~Latitude,    # Set longitude and latitude
  #         icon = ~icons(             # Define custom icon properties
  #           iconUrl = "https://as1.ftcdn.net/v2/jpg/06/70/58/68/1000_F_670586814_zPsLZ38T5wtVC4vDKIGNHCN8aYXPSqo1.webp",
  #           iconWidth = ~size_category, 
  #           iconHeight = ~size_category,
  #           iconAnchorX = ~size_category,
  #           iconAnchorY = ~size_category,
  #           popupAnchorX = 0,
  #           popupAnchorY = -size_category
  #         ),
  #         label = ~Location,             # Label each marker
  #         popup = ~paste0("<b>", Location, "</b><br>Amount: ", n) 
  #       )
  #   }
  # })
  
  # # Reactive expression for mean calculation
  # meanData <- reactive({
  #   with(df, tapply(value, category, mean))
  # })
  # 
  # # Reactive expression for median calculation
  # medianData <- reactive({
  #   with(df, tapply(value, category, median))
  # })
  # 
  # # Render the bar plot (mean vs median)
  # output$barplot <- renderPlot({
  # 
  #   if (input$plotType == "Mean") {
  #     ggplot(df, aes(x = category, y = value)) +
  #       geom_bar(stat = "identity", fill = "blue", color = "black") +
  #       geom_errorbar(aes(ymin = value - stddev, ymax = value + stddev), width = 0.4,
  #                     position = position_dodge(0.9)) +
  #       labs(title = "Bar Plot of Means with Error Bars", 
  #            x = "Category", y = "Value") 
  #   } 
  # 
  #   else if (input$plotType == "Median") {
  #     ggplot(df, aes(x = category, y = value)) +
  #       geom_bar(stat = "identity", fill = "blue", color = "black") +
  #       labs(title = "Bar Plot of Medians without Error Bars", 
  #            x = "Category", y = "Value") 
  #   }
  # })
  
  # Initialize music state
  observe({
    if (input$toggle_music) {
      session$sendCustomMessage(type = "playMusic", message = list(volume = 0.5))
    } else {
      session$sendCustomMessage(type = "stopMusic", message = list())
    }
  })
  
  
  # Define action for "Fish Puns" button
  observeEvent(input$fish_puns_btn, {
    # Randomly select a fish pun
    random_pun <- sample(fish_puns, 1)

    # Show the pun in a pop-up
    showModal(
      modalDialog(
        title = "Fish Pun",
        random_pun,
        easyClose = TRUE,
        footer = NULL
      )
    )
  })
}