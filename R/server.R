#' @describeIn launch_app updated server function
#' @export
rb_server <- function(input, output, session) {
  
  .salaries <- filters_server('filters')
  
  value_boxes_stats_server('value_boxes', .salaries)
  
  output$plot_salary_histogram <- plotly::renderPlotly({
    if (nrow(.salaries()) == 0 ) return(NULL)
    
    plot_salary_histogram(.salaries(), salary_total)
  })
  
  
  output$table_career_progression <- gt::render_gt({
    gt_career_progression(.salaries())
  })
  output$plot_experience <- plotly::renderPlotly({
    
    # suppress warnings for app session
    storeWarn<- getOption("warn")
    options(warn = -1)
    
    if (nrow(.salaries()) > 0) {
      plot_experience(.salaries())
    } else {
      p <- ggplot() + theme_minimal()
      plotly::ggplotly(p)
    }
  })
  
  # render the table + other components
  table_raw_server('table_raw', .salaries, height = gt::px(400))
  
}