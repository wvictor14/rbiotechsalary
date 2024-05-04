rb_server <- function(input, output, session) {
  
  .filters <- filters_server('filters')
  
  .salaries <- reactive({
    
    # handle when no granular is selected
    if (is.null(.filters()$location_granular)) {
      loc_gran <- levels(salaries$location_granular)
    } else {
      loc_gran <- .filters()$location_granular
    }
    
    salaries |>
      filter(
        lubridate::year(date) %in% .filters()$.date,
        title_general %in% .filters()$title,
        location_country %in% .filters()$location_country,
        location_granular %in% loc_gran
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
  
  value_boxes_stats_server('value_boxes',  reactive(.salaries_hist_clicked()))
  
  # plot salary histogram ----
  salary_hist_data <- reactive({
    .salaries() |>  make_hist_data(salary_total)
  })
  
  output$plot_salary_histogram <- plotly::renderPlotly({
    if (nrow(.salaries()) == 0 ) return(NULL)
    plot_salary_histogram(salary_hist_data(), source = 'hist')
  })
  
  
  # on click return selected data, otherwise return unfiltered
  
  # reset range when inputs change (e.g. change title, location, etc.)
  .salaries_hist_range <- reactiveValues(min = -Inf, max = -Inf)
  
  observeEvent(.salaries(), {
    .salaries_hist_range$min <- -Inf
    .salaries_hist_range$max <- Inf
  })
  
  observeEvent(
    plotly::event_data("plotly_click", source = 'hist'), 
    ignoreNULL = FALSE, 
    {
      # filter plot data based on click event
      click_event <- plotly::event_data("plotly_click", source = 'hist')
      if (!is.null(click_event)) { 
        click_data <- salary_hist_data()$data |> 
          filter(mids_lab == plotly::event_data("plotly_click", source = 'hist')$x)
      } else {
        click_data <- list(X2 = -Inf, X1 = Inf)
      }
      
      .salaries_hist_range$min <- click_data$X2
      .salaries_hist_range$max <- click_data$X1
    })
  
  .salaries_hist_clicked <- reactive({
    .salaries() |>  
      filter(
        salary_total >= .salaries_hist_range$min,
        salary_total <= .salaries_hist_range$max
      )
  })
  
  output$click <- renderPrint({
    .salaries_hist_clicked()[1:3,1:3]
  })
  
  # table data panel ----
  table_raw_server('table_raw', .salaries_hist_clicked) # connected to histogram
  
  
  # raw data pipe
  output$skim_raw_data <- reactable::renderReactable({
    skim_raw_data |>  
      reactable_rbs(
        fullWidth = FALSE,
        columns = list(
          n_missing = reactable::colDef(name = 'Missing', width = 120),
          
          skim_variable = reactable::colDef(
            name = 'Survey Response Variable',
            width = 375
          ),
          
          complete_rate = reactable::colDef(
            name = 'Complete',
            width = 100,
            format = reactable::colFormat(percent = TRUE, digits = 1))
        ))
  })
  
  # career progression
  output$plot_career_progression <- plotly::renderPlotly({
    plot_career_progression(.salaries(), bg_color = "rgba(0, 0, 0, 0)")
  })
  
}