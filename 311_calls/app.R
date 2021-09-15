library(shiny)
library(tidyverse)
library(plotly)

theme_set(theme_minimal())

# Read in daily forecasts:
forecasts_daily <- readr::read_csv('forecasts_daily.csv')
best_models<- readr::read_csv('best_models.csv')

# Get unique agency names and complaint types:
agency_names <- unique(forecasts_daily$agency)
complaint_types <- unique(forecasts_daily$complaint_type)


# Define function for plotting:
plot_ts <- function(.data, .agency, .complaint_type, best_models){
    
    .title <- paste0('Daily calls for "', .complaint_type, '" at ', .agency)
    .subtitle <- best_models %>% 
        filter(agency == .agency, complaint_type == .complaint_type) %>%
        pull(best_model) %>% paste0("Modeled using ", .)
    .todays_date <- .data %>% 
        filter(complaint_type == .complaint_type, agency == .agency) %>% 
        na.omit() %>% 
        pull(date) %>% min()
    
    p <- .data %>% 
        filter(agency == .agency, complaint_type == .complaint_type) %>% 
        ggplot(aes(x = date)) +
        geom_ribbon(aes(ymin = lower_80, ymax = upper_80),
                    fill = 'grey 80') +
        geom_line(aes(y = .mean), color = 'grey20') +
        geom_point(aes(y = .mean), fill = 'grey20') +
        geom_vline(xintercept = as.numeric(.todays_date), linetype = 'dashed', color = 'blue') +
        scale_x_date(date_breaks = '1 week', date_labels = '%b %d') +
        labs(title = NULL,
             caption = .subtitle,
             x = NULL,
             y = 'Count of daily calls')
    
    plotly::ggplotly(p) %>% config(displayModeBar = F)
}

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Daily and Weekly Forecasts of NYC 311 Calls"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("agency",
                        "Select Agency:",
                        choices = agency_names),
            selectInput("complaint_type",
                        "Select Complaint Type:",
                        choices = complaint_types)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            uiOutput("summary"),
           plotly::plotlyOutput("tsplot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    observeEvent(input$agency, {
        new_complaint_types = forecasts_daily %>% 
            filter(agency == input$agency) %>%
            pull(complaint_type) %>%
            unique() %>% 
            sort()
        
        updateSelectInput(inputId = "complaint_type", choices = new_complaint_types)
    })
    
    output$tsplot <- plotly::renderPlotly({
        plot_ts(forecasts_daily, input$agency, input$complaint_type, best_models)
    })
    
    output$summary <- renderUI({
        
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
        
        text_string <- HTML(paste0(scales::comma_format()(one_step_fcst)," calls forecasted for today <br>", 
                              scales::comma_format()(weekly_avg), " calls on average forecasted daily for the next week"))
            
        return(text_string)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)


# NOTES: 
# 1) Fix problem with NAs in pipeline (mean impute and create another column in dataframe 
# to forecast based on imputed values, but do not show in graphs)

# 2) Format and design changes.
