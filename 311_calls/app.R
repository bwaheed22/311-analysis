# RShiny App to display daily forecasts of 311 calls for NYC Agencies:

library(shiny)
library(shinyWidgets)
library(tidyverse)
library(plotly)
library(leaflet)
library(ggthemes)

theme_set(theme_fivethirtyeight(base_size = 10,
                                base_family = 'arial'))

# Read in daily forecasts, best models, and yesterday's actuals data frames:
forecasts_daily <- readr::read_csv('forecasts_daily.csv')
best_models<- readr::read_csv('best_models.csv')
yest_data <- readr::read_csv('yesterday_data.csv') %>% 
    mutate(complaint_type = tolower(complaint_type))

# Get unique agency names and complaint types:
agency_names <- unique(forecasts_daily$agency)
complaint_types <- unique(forecasts_daily$complaint_type)

# Define function for base map:
base_map <- leaflet(yest_data) %>% 
    addProviderTiles(providers$CartoDB.Voyager,
                     options = providerTileOptions(noWrap = TRUE)) %>%
    setView(lng = -73.98928, lat = 40.75042, zoom = 10)


# Define function for plotting:
plot_ts <- function(.data, .agency, .complaint_type, best_models){
    
    .title <- paste0('Historical and Projected Service Calls for "', .complaint_type, '" at ', .agency)
    .subtitle <- best_models %>% 
        filter(agency == .agency, complaint_type == .complaint_type) %>%
        pull(best_model) %>% paste0("Forecasts calculated using '", ., "' modeling")
    .data <- filter(.data, complaint_type == .complaint_type, agency == .agency)
    .todays_date <- .data %>% 
        na.omit() %>% 
        pull(date) %>% 
        min()
    
    # construct plot
    p <- .data %>% 
        mutate(lower_80 = if_else(is.na(lower_80), .mean, lower_80),
               upper_80 = if_else(is.na(upper_80), .mean, upper_80)) %>% 
        ggplot(aes(x = date)) +
        geom_ribbon(aes(ymin = lower_80, ymax = upper_80),
                    fill = 'red', alpha = 0.15) +
        geom_line(aes(y = .mean), color = 'grey20') +
        geom_point(aes(y = .mean, text = paste0('Daily calls: ', round(.mean))), fill = 'grey20') +
        geom_vline(xintercept = as.numeric(.todays_date), linetype = 'dashed', color = 'grey50') +
        scale_x_date(date_breaks = '1 day', date_labels = '%b %d') +
        lims(y = c(0, NA)) +
        labs(title = .title,
             x = 'Date',
             y = 'Daily Number of Calls') + 
        theme(axis.text.x = element_text(angle = -40, hjust = 0, size = 7))
    
    # convert from ggplot to plotly
    p <- p %>% 
        plotly::ggplotly(tooltip = 'text') %>% 
        config(displayModeBar = F) %>% 
        layout(xaxis = list(fixedrange = TRUE), 
               yaxis = list(fixedrange = TRUE), 
               font = list(family = "Arial"),
               annotations = list(x = 0.002, y = 1.09, text = .subtitle,
                                  showarrow = F, xref='paper', yref='paper',
                                  xanchor='left', yanchor='auto', xshift=0, yshift=0,
                                  font = list(size = 12, color = 'grey', family = 'Arial')))
    
    return(p)
}


# Define application UI:
ui <- fluidPage(
    
    # load custom CSS file
    includeCSS("www/311.css"),
    
    # Application title
    titlePanel("Daily Forecasts of 311 Service Requests for New York City Agencies"),
    
    # Sidebar with input for agency and complaint types:
    sidebarLayout(
        sidebarPanel(
            selectInput("agency",
                        "Select Agency:",
                        choices = agency_names),
            pickerInput(inputId = "complaint_type",
                        "Select Complaint Type:",
                        choices = complaint_types),
            
            # Show map of yesterday's data:
            leafletOutput("dailymap", width = "100%", height = "500px"),
            br(),
            actionButton("reset_button", "Reset View"),
            HTML('<br><br>Data updated daily')
        ),
        
        # Show a plot of the generated distribution with a description of 
        # the forecasts:
        mainPanel(
            uiOutput("summary"),
            tabsetPanel(type = "tabs",
                        tabPanel("Forecast Plot", plotly::plotlyOutput("tsplot")),
                        tabPanel("Yesterday Data", DT::dataTableOutput("table")),
                        tabPanel("Weather Map"))
        )
    )
)

# Define functions to plot historical data and future forecasts:
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

# Run the application 
shinyApp(ui = ui, server = server)


# NOTES: 
# 1) Fix problem with NAs in pipeline (mean impute and create another column in dataframe 
# to forecast based on imputed values, but do not show in graphs)

# 2) Format and design changes.
