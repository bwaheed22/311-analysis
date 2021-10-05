# Define function for plotting:
plot_ts <- function(.data, .agency, .complaint_type, best_models){
  
  .title <- paste0('Historical and Projected Service Calls for "', .complaint_type, '" at ', .agency)
  .subtitle <- best_models %>% 
    filter(agency == .agency, complaint_type == .complaint_type) %>%
    pull(best_model) %>% 
    paste0("Forecasts calculated using '", ., "' modeling")
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
    geom_vline(xintercept = as.numeric(Sys.Date()), linetype = 'dashed', color = 'grey50') +
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
