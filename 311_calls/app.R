# RShiny App to display daily forecasts of 311 calls for NYC Agencies:

library(shiny)
library(shinyWidgets)
library(tidyverse)
library(plotly)

theme_set(theme_minimal())

# Read in daily forecasts and best models data frames:
forecasts_daily <- readr::read_csv('forecasts_daily.csv')
best_models<- readr::read_csv('best_models.csv')

# Get unique agency names and complaint types:
agency_names <- unique(forecasts_daily$agency)
complaint_types <- unique(forecasts_daily$complaint_type)


# Define function for plotting:
plot_ts <- function(.data, .agency, .complaint_type, best_models){
    
    # .title <- paste0('Daily calls for "', .complaint_type, '" at ', .agency)
    .subtitle <- best_models %>% 
        filter(agency == .agency, complaint_type == .complaint_type) %>%
        pull(best_model) %>% paste0("Forecasts calculated using '", ., "' modeling")
    
    .todays_date <- .data %>% 
        filter(complaint_type == .complaint_type, agency == .agency) %>% 
        na.omit() %>% 
        pull(date) %>% 
        min()
    
    p <- .data %>% 
        filter(agency == .agency, complaint_type == .complaint_type) %>% 
        ggplot(aes(x = date)) +
        geom_ribbon(aes(ymin = lower_80, ymax = upper_80),
                    fill = 'grey80') +
        geom_line(aes(y = .mean), color = 'grey20') +
        geom_point(aes(y = .mean), fill = 'grey20') +
        geom_vline(xintercept = as.numeric(.todays_date), linetype = 'dashed', color = 'blue') +
        scale_x_date(date_breaks = '1 week', date_labels = '%b %d') +
        labs(title = NULL,
             caption = .subtitle,
             x = 'Date',
             y = 'Daily Number of Calls') + 
        lims(y = c(0, NA))
    
    plotly::ggplotly(p) %>% config(displayModeBar = F) %>% 
        layout(xaxis = list(fixedrange = TRUE), 
               yaxis = list(fixedrange = TRUE), font = list(family = "Arial"),
               annotations = list(x = 1, y = -0.1, text = .subtitle,
                                  showarrow = F, xref='paper', yref='paper',
                                  xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                  font = list(size = 12, color = 'red', family = 'Arial')))
}

# Define UI for application that draws a histogram
ui <- fluidPage(

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
                        choices = complaint_types)
        ),

        # Show a plot of the generated distribution with a description of 
        # the number of forecasted calls:
        mainPanel(
            uiOutput("summary"),
           plotly::plotlyOutput("tsplot")
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
    
    output$tsplot <- plotly::renderPlotly({
        plot_ts(forecasts_daily, input$agency, input$complaint_type, best_models)
    })
    
    output$summary <- renderUI({
        
        .yest_date <- forecasts_daily %>% 
            filter(complaint_type == input$complaint_type, agency == input$agency) %>% 
            na.omit() %>% 
            pull(date) %>% 
            min()-1
        
        .yest_total_calls <- forecasts_daily %>% 
            filter(date == .yest_date) %>% 
            summarize(n = round(sum(.mean),0)) %>% 
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
        
        text_string <- HTML(paste0("<br> Yesterday, the City received a total of ", 
                                   scales::comma_format()(.yest_total_calls)," service calls and <br>",
                                   input$agency, " received ", scales::comma_format()(.yest_agency_total_calls), 
                                   " service calls.  <br> <br> Today they can expect ", 
                                   scales::comma_format()(one_step_fcst)," service calls related to '", 
                                   input$complaint_type, "'. <br> <br>", 
                              "On average, there will be ", scales::comma_format()(weekly_avg), 
                              " service calls daily for ", "'", input$complaint_type,"'",
                              " over the next week. <br> <br>"))
            
        return(text_string)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)


# NOTES: 
# 1) Fix problem with NAs in pipeline (mean impute and create another column in dataframe 
# to forecast based on imputed values, but do not show in graphs)

# 2) Format and design changes.
