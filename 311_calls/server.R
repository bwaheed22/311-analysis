server <- function(input, output, session) {
  
  observeEvent(input$agency, {
    new_complaint_types = forecasts_daily %>% 
      filter(agency == input$agency) %>%
      pull(complaint_type) %>%
      unique() %>% 
      sort()
    
    disabled_choices <- !complaint_types %in% new_complaint_types
    # 
    updatePickerInput(session = session,
                      inputId = "complaint_type",
                      choices = complaint_types,
                      choicesOpt = list(
                        disabled = disabled_choices,
                        style = ifelse(disabled_choices,
                                       yes = "color: rgba(119, 119, 119, 0.5);",
                                       no = "")))
    
  })
  
  output$dailymap <- renderLeaflet(base_map)
  
  observe({
    yest_data <- yest_data %>% 
      filter(agency == input$agency, 
             complaint_type == input$complaint_type)
    
    leafletProxy("dailymap", session) %>%
      clearMarkerClusters() %>% 
      addCircleMarkers(
        clusterOptions = markerClusterOptions(),
        lng = jitter(yest_data$longitude, factor = 2),
        lat = jitter(yest_data$latitude, factor = 2),
        radius = 3,
        color = ifelse(is.na(yest_data$closed_date), 'blue', 'red'),
        stroke = TRUE,
        fillOpacity = 1,
        popup = paste0(
          "<b> Incident Description: </b> <br>", yest_data$descriptor, "<br>",
          "<b> Community Board: </b>", as.character(yest_data$community_board), "<br>",
          "<b> Date: </b>", as.character(yest_data$created_date), "<br>",
          "<b> Incident Address: </b>", as.character(yest_data$incident_address)))
    
    input$reset_button
    
    leafletProxy("dailymap") %>% 
      setView(lng = -73.98928, lat = 40.75042, zoom = 10)
    
  })
  
  output$tsplot <- plotly::renderPlotly({
    plot_ts(forecasts_daily, input$agency, input$complaint_type, best_models)
  })
  
  output$table <- DT::renderDataTable(yest_data,
                                      rownames = FALSE,
                                      options = list(
                                        pageLength = 5, # sets n observations shown
                                        lengthChange = FALSE, #  removes option to change n observations shown
                                        sDom  = '<"top">lrt<"bottom">ip',  # removes the search bar
                                        scrollX = TRUE # enable side scroll so table doesn't overflow
                                      ))
  
  output$summary <- renderUI({
    
    .yest_date <- forecasts_daily %>% 
      filter(complaint_type == input$complaint_type, agency == input$agency) %>% 
      na.omit() %>% 
      pull(date) %>% 
      min()-1
    
    .yest_total_calls <- forecasts_daily %>% 
      filter(date == .yest_date) %>%
      summarize(n = round(sum(.mean, na.rm = T),0)) %>% 
      pull(n)
    
    .yest_agency_total_calls <- forecasts_daily %>% 
      filter(agency == input$agency, date == .yest_date) %>% 
      summarize(n = round(sum(.mean),0)) %>% 
      pull(n)
    
    forecasts_daily <- forecasts_daily %>%
      filter(agency == input$agency, complaint_type == input$complaint_type)
    
    one_step_fcst <- forecasts_daily %>%
      na.omit() %>%
      filter(date == min(date)) %>% 
      pull(.mean)
    
    weekly_avg <- forecasts_daily %>%
      na.omit() %>% 
      summarise(mean = mean(.mean)) %>% 
      pull(mean)
    
    text_string <- HTML(paste0("<br> <ul> <li>Yesterday, the <b> City received a total of ", 
                               scales::comma_format()(.yest_total_calls)," service calls </b> and <b>",
                               input$agency, " received ", scales::comma_format()(.yest_agency_total_calls), 
                               " service calls. </b> </li> <li> Today, <b>",input$agency," can expect ", 
                               scales::comma_format()(one_step_fcst)," service calls related to '", 
                               input$complaint_type, "'. </b> </li>", 
                               "<li> On average, there will be ", scales::comma_format()(weekly_avg), 
                               " service calls daily for ", "'", input$complaint_type,"'",
                               " over the next week.</li></ul> <br> <br>"))
    
    return(text_string)
  })
  
  # shut down R after closing browser
  session$onSessionEnded(function() {
    stopApp()
  })
}