#' Value boxes ui
#' @export
value_boxes_stats_ui <- function(id, bg = '#1E2122', fg = '#41AB5DFF') {
  
  layout_column_wrap(
    #width = '350px',
    fixed_width = FALSE,
    value_box(
      title = "Average Salary", 
      value = uiOutput(NS(id, 'text_average')),
      #theme= 'text-success',
      theme = value_box_theme(bg = bg, fg = fg),
      showcase = plotly::plotlyOutput(NS(id, 'plot_sparkline_average')), 
      showcase_layout = "left center", 
      full_screen = TRUE,
      fill = FALSE, 
      #height = NULL,
      height = '150px',
      uiOutput(NS(id, 'text_ave_breakdown'))
    ),
    value_box(
      title = "Number of Survey Respondents", 
      value = textOutput(NS(id, 'n_respondents')),
      theme = value_box_theme(bg = bg, fg = fg),
      showcase =plotly::plotlyOutput(NS(id, 'plot_sparkline_users')),
      showcase_layout = "left center", 
      full_screen = TRUE,
      height = '150px',
      fill = FALSE
    ),
    value_box(
      title = "Average Years of Experience", 
      value = textOutput(NS(id, 'text_ave_yoe')),
      theme = value_box_theme(bg = bg, fg = fg),
      showcase = bsicons::bs_icon('clock-history'),
      showcase_layout = "left center", 
      full_screen = TRUE,
      fill = FALSE,
      height = '150px'
    )
  )
}
#' Value boxes server
#' @export
value_boxes_stats_server <- function(id, .salaries) {
  stopifnot(is.reactive(.salaries))
  moduleServer(id, function(input, output, session) {
    
    # value box 1
    output$plot_sparkline_average <- plotly::renderPlotly({
      if (nrow(.salaries()) == 0 ) return(NULL)
      
      .salaries() |>
        # take most recent 100 submissions for sparkline
        arrange(desc(date)) |> 
        slice(1:100) |> 
        plot_sparkline(x = date, y = salary_total, color = '#41AB5DFF')
    })
    
    stats <- reactive({
      .salaries() |> 
        select(salary_total, salary_base, bonus) |> 
        summarize(across(everything(), ~mean(.x, na.rm = TRUE))) |>
        rename_with(
          ~stringr::str_remove(.x, 'salary_') |>  stringr::str_to_sentence(), 
          everything()
        ) |> 
        tidyr::pivot_longer(everything()) |> 
        mutate(
          .label = glue::glue("{name}: {round(value, digits = -3)/1000}K")
        )
    })
    
    output$text_average <- renderUI({
      .text <- stats() |> 
        filter(name == 'Total') |> pull(value) |>  round(digits = -3) |> 
        scales::dollar()
      if (is.na(.text)) { .text <- 'No data.'}
      HTML(paste0(.text, "<br>"))
    })
    
    output$text_ave_breakdown <- renderUI({
      if (any(is.na(stats()$value))) { return(HTML('')) }
      HTML(
        stats() |> filter(name != 'Total') |> pull(.label) |>  paste0(collapse = ', ') 
      )
    })
    
    # value box 2 ----
    output$plot_sparkline_users <- plotly::renderPlotly({
      if (nrow(.salaries()) == 0 ) return(NULL)
      .salaries() |>  
        count(date) |>  
        mutate(x = cumsum(n))  |> 
        plot_sparkline(date, x, color = '#41AB5DFF')
    })
    
    
    output$n_respondents <- renderText({
      if (nrow(.salaries()) == 0) { return(HTML('')) }
      nrow(.salaries())
    })
    
    
    # value box 3 ----
    output$text_ave_yoe <- renderText({
      if (nrow(.salaries()) == 0) { return(HTML('')) }
      .salaries() |>  pull(years_of_experience) |> mean(na.rm = TRUE) |> 
        round(digits = 1)
    })
    
  })
}