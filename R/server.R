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
   content_title <- reactive(
    glue::glue(
      "{role} Salaries in {country}",
      role = .filters()$title,
      country = .filters()$location_country
    ) |>  as.character()
  )
  
  output$content_title_1 <- renderText(content_title())
  output$content_title_2 <- renderText(content_title())
  
  value_boxes_stats_server('value_boxes', .salaries)
  output$plot_salary_histogram <- plotly::renderPlotly({
    if (nrow(.salaries()) == 0 ) return(NULL)
    plot_salary_histogram(.salaries(), salary_total)
  })
  
  # raw data panel
  table_raw_server('table_raw', .salaries)
  
  # career progression
  output$plot_career_progression <- plotly::renderPlotly({
    plot_career_progression(.salaries(), bg_color = "rgba(0, 0, 0, 0)")
  })
  
}