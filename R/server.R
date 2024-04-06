#' @describeIn launch_app updated server function
#' @export
rb_server <- function(input, output, session) {
  
  .filters <- filters_server('filters')
  
  .salaries <- reactive({
    salaries |>
      filter(
        lubridate::year(date) %in% .filters()$.date,
        title_general %in% .filters()$title,
        location_country %in% .filters()$location_country,
        location_granular %in% .filters()$location_granular
      )
  })
  
  # panel 1 ----
  output$content_title <- renderText(
    glue::glue(
      "{role} Salaries in {country}",
      role = .filters()$title,
      country = .filters()$location_country
    ) |>  as.character()
  )
  
  value_boxes_stats_server('value_boxes', .salaries)
  output$plot_salary_histogram <- plotly::renderPlotly({
    if (nrow(.salaries()) == 0 ) return(NULL)
    plot_salary_histogram(.salaries(), salary_total)
  })
  table_raw_server('table_raw', .salaries, height = gt::px(400))
  
  # panel 2 ----
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
}