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