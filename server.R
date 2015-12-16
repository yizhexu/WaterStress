shinyServer(function(input, output, session) {
  
  output$link <- renderUI({
    HTML('<a href="http://developer.awhere.com/api/get-started/"> Need an account? Click here </a>')
  })
  
  ####################
  #### Map Output ####
  ####################
  
  output$map <- renderLeaflet({
    
    map <- leaflet::leaflet() %>% 
      addTiles() %>%
      addMarkers(lng = input$lng,
                 lat = input$lat, 
                 icon = aWhereIcon,
                 popup = as.character(tagList(
                   sprintf(paste0("Latitude: ", as.numeric(input$lat))), tags$br(),
                   sprintf(paste0("Longitude: ", as.numeric(input$lng))), tags$br()
                 ))) %>%
      setView(lng = input$lng, 
              lat = input$lat,
              zoom = 12)
  })
  
  output$baseline <- renderLeaflet({
    if(input$generatemap){
      baseline <- leaflet() %>%
        addTiles() %>%
        addRectangles(lng1 = rectangle()[, 1], lat1 = rectangle()[, 2],
                      lng2 = rectangle()[, 3], lat2 = rectangle()[, 4],
                      fillColor = pal(baseline_index()), fillOpacity = 0.6, stroke = FALSE) %>%
        addLegend(pal = pal, values = c(baseline_index(), target_index()), opacity = 0.6)
    }
  })
  
  output$target <- renderLeaflet({
    if(input$generatemap){
      target <- leaflet() %>%
        addTiles() %>%
        addRectangles(lng1 = rectangle()[, 1], lat1 = rectangle()[, 2],
                      lng2 = rectangle()[, 3], lat2 = rectangle()[, 4],
                      fillColor = pal(target_index()), fillOpacity = 0.6, stroke = FALSE) %>%
        addLegend(pal = pal, values = c(baseline_index(), target_index()), opacity = 0.6)
    }
  })
  
  output$diff <- renderLeaflet({
    if(input$generatemap){
      # leaf_diff <- 
      leaflet() %>%
        addTiles() %>%
        addRectangles(lng1 = rectangle()[, 1], lat1 = rectangle()[, 2],
                      lng2 = rectangle()[, 3], lat2 = rectangle()[, 4],
                      fillColor = pal2(target_index() - baseline_index()), fillOpacity = 0.6, stroke = FALSE) %>%
        addLegend(pal = pal2, values = c(target_index() - baseline_index()), opacity = 0.6)
    }
  })
  
  ##########################
  #### Data Calculation ####
  ##########################
  
  # calculate dates
  
  start_date_baseline <- reactive({
    paste(input$baseline_year, month(input$start_date), day(input$start_date), sep = "-")
  })
  
  end_date_baseline <- reactive({
    paste(input$baseline_year, month(input$end_date), day(input$end_date), sep = "-")
  })
  
  start_date_target <- reactive({
    paste(input$target_year, month(input$start_date), day(input$start_date), sep = "-")
  })
  
  end_date_target <- reactive({
    paste(input$target_year, month(input$end_date), day(input$end_date), sep = "-")
  })
  
  # calculate index
  baseline_index <- reactive({
    calculate_index(start_date_baseline(), end_date_baseline(), data()[[1]])
  })
  
  target_index <- reactive({
    calculate_index(start_date_target(), end_date_target(), data()[[2]])
  })
  
  # calculate coordinates
  coors <- reactive({
    calculate_coor(start_date_baseline(), data()[[1]])
  })
  
  # aggregate to rectangles
  rectangle <- reactive({
    do.call(rbind, lapply(1:dim(coors())[1], function(i) {
      create_rectangle(coors(), i)}
    ))
  })
  
  # Get data from api
  
  data <- reactive({
    # fetch data if fetch data button is pressed
    if(input$fetchdata) {
      
      if ( "token" %in% ls() | "expire_time" %in% ls()) {
        if (expire_time > Sys.time()) {
          token
        } else {
          token <- get_token(input$api_key, input$api_code)
        }
      } else {
        token <- get_token(input$api_key, input$api_code)
      }
      
      query_baseline <- create_query(input$lat, input$lng, x = 5, y = 5, size = 5, 
                                     date = set_date(start_date_baseline(), end_date_baseline()), 
                                     attribute = attribute)
      query_target <- create_query(input$lat, input$lng, x = 5, y = 5, size = 5, 
                                   date = set_date(start_date_target(), end_date_target()), 
                                   attribute = attribute)
      
      withProgress(message = "Querying data in progress", detail = "Getting data for:", value = 0, {
        lapply(1:2, function(i) {
          query <- c("query_baseline", "query_target")
          name <- c("baseline", "target")
          
          # progress bar update
          incProgress(0.5, detail = paste("Getting data for:", paste(name[i], "time period")))
          
          get_weather(token, get(query[i]))
        })
      })
    } else {
      list(baseline, target)
    }
  })
  
  ##############################
  #### Datatable Generation ####
  ##############################
  
  output$tbl_a = DT::renderDataTable({
    if(input$fetchdata) {
      data.frame(coordinates = coors(), 
                 baseline = baseline_index(), 
                 target = target_index(), 
                 difference = target_index() - baseline_index())
    }
  }, 
  rownames = FALSE, 
  caption = "Table 1: Water Stress Index")

  #########################
  #### Text Generation ####
  #########################
  
  output$baseline_sub <- renderUI({
    if(input$generatemap){
      paragraph <- paste("Precipitation over Potential Evapotranspiration for location", 
                         paste0("(", input$lat, ", ", input$lng, ")"), 
                         "from", start_date_baseline(), "to", end_date_baseline())
      
      list(tags$br(), 
           tags$h4("Water Stress Index - Baseline"), 
           tags$p(paragraph), 
           tags$br())
    }
  })
  
  output$target_sub <- renderUI({
    if(input$generatemap){
      paragraph <- paste("Precipitation over Potential Evapotranspiration for location", 
                         paste0("(", input$lat, ", ", input$lng, ")"), 
                         "from", start_date_target(), "to", end_date_target())
      
      list(tags$br(), 
           tags$h4("Water Stress Index - Target"),
           tags$p(paragraph), 
           tags$br())
    }
  })
  
  output$diff_sub <- renderUI({
    if(input$generatemap){
      paragraph <- paste("The difference of Water Stress Index between", 
                         start_date_baseline(), "to", end_date_baseline(), "and",
                         start_date_target(), "to", end_date_target() )
      
      list(tags$br(), 
           tags$h4("Water Stress Index - Change"), 
           tags$p(paragraph),
           tags$br())
    }
  })
  
})