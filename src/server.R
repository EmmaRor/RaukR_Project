# Define server logic
server <- function(input, output, session) {
  
  # Read data file
  data <- read.csv("../Results_20232019.csv")
  fish_puns <- readLines("../data/fish_puns.txt")
  
  # These things are only done once
  species <- unique(data$Species)
  species <- species[order(species)]
  updateSelectInput(session, "species", choices = species, selected = "Perch")
  
  max_year <- max(data$Year)
  min_year <- min(data$Year)
  middle_year <- round((max_year + min_year) / 2, 0)
  updateSliderInput(session, "year",
                    min = min_year - 1,
                    max = max_year,
                    value = middle_year)
  
  
  # Update dataframe based on choices
  reactive_data <- reactive({
    df <- data[which(data$Species == input$species & data$Year == input$year), ]
    
    if (is.null(df) || nrow(df) == 0) {
      shinyalert(title = "Error",
                 text = paste("Something is fishy here. No data for", input$species, "in", input$year),
                 type = "error")
      
      return(NULL)
    }
    
    return(df)
  })
  
  df_col <- reactive({
    if (input$indicator == "Abundance"){
      df_col <- "CPUE"
    } else if (input$indicator == "Size"){
      df_col <- "L90_cm"
    }
    
    return(df_col)
  })
  
  reactive_marker_data <- reactive({
    df <- reactive_data()
    indicator <- input$indicator
    
    if (is.null(df) || nrow(df) == 0) {
      return(NULL)
    }
    
    if (indicator == "Abundance"){
      display_df_sizes <- df %>%
        mutate(size_category = case_when(
          CPUE > (0.75 * max(df$CPUE)) ~ 20,
          CPUE > (0.5 * max(df$CPUE)) ~ 15,
          CPUE > (0.25 * max(df$CPUE)) ~ 10,
          TRUE ~ 5
        ))
    } else if (indicator == "Size"){
      shinyalert(title = "Warning",
                 text = paste("Your data is fishy! To use the size indicator, every location with less than 50", input$species, "caught have been removed"),
                 type = "warning")
      
      display_df <- df %>%
        subset(!is.na(L90_cm))
      
  
      display_df_sizes <- display_df %>%
        mutate(size_category = case_when(
          L90_cm > (0.75 * max(df$L90_cm)) ~ 20,
          L90_cm > (0.5 * max(df$L90_cm)) ~ 15,
          L90_cm > (0.25 * max(df$L90_cm)) ~ 10,
          TRUE ~ 5
        ))
    }

    return(display_df_sizes)
  })
  
  # The map
  output$swedenMap <- renderLeaflet({
    leaflet() %>%                                     # Create a leaflet map object
      addTiles() %>%                                  # Default OpenStreetMap tiles
      setView(lng = 18.6435, lat = 60.1282, zoom = 5) # Somewhere in Sweden
  })
  
  # The marker (fish vs circles)
  observe({
    df <- reactive_marker_data()
    marker <- input$marker
    
    if (is.null(df) || nrow(df) == 0) {
      leafletProxy("swedenMap", data = df) %>%
        clearMarkers()
      
      return(NULL)
    }
    
    if (marker == "Circles") {
      leafletProxy("swedenMap", data = df) %>%
        clearMarkers() %>%
        addCircleMarkers(
          lng = ~Longitude, lat = ~Latitude,     # Set longitude and latitude
          label = ~Location,                     # Label each marker,
          color = "#FA8072",
          radius = ~size_category,               # Radius size
          stroke = FALSE, fillOpacity = 0.6,     # Border and fill opacity
          popup = ~paste0("<b>", Location, "</b><br>Amount: ", count, "</b><br>Abundance: ", round(CPUE, 2))
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
          popup = ~paste0("<b>", Location, "</b><br>Amount: ", count, "</b><br>Abundance: ", round(CPUE, 2))
        )
    }
  })
  
  # Render legend items dynamically based on species and year
  output$legendSmall <- renderUI({
    legend_text <- tryCatch({
      paste("Small: 0 -", round(0.25 * max(reactive_marker_data()[, df_col()]), 0))
    }, error = function(e) {
      paste("No data available")
      })
    div(class = "legend-item",
        div(class = "legend-circle small"),
        legend_text)
  })
  
  output$legendMedium <- renderUI({
    legend_text <- tryCatch({
      paste("Medium: ", round(0.25 * max(reactive_marker_data()[, df_col()]), 0), "-", round(0.5 * max(reactive_marker_data()[, df_col()]), 0))
    }, error = function(e) {
      paste("No data available")
    })
    div(class = "legend-item",
        div(class = "legend-circle medium"),
        legend_text)
  })
  
  output$legendLarge <- renderUI({
    legend_text <- tryCatch({
      paste("Large: ", round(0.5 * max(reactive_marker_data()[, df_col()]), 0), "-", round(0.75 * max(reactive_marker_data()[, df_col()]), 0))
    }, error = function(e) {
      paste("No data available")
    })
    div(class = "legend-item",
        div(class = "legend-circle large"),
        legend_text)
  })
  
  output$legendExtraLarge <- renderUI({
    legend_text <- tryCatch({
      paste("Extra Large: ", round(0.75 * max(reactive_marker_data()[, df_col()]), 0), "-", round(max(reactive_marker_data()[, df_col()]), 0))
    }, error = function(e) {
      paste("No data available")
    })
    div(class = "legend-item",
        div(class = "legend-circle extra-large"),
        legend_text)
  })
  
  
  
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