#' Value boxes ui
#' @export
value_boxes_stats_ui <- function(
    id, bg = '#1E2122', fg = '#41AB5DFF',
    showcase_layout = 'left center',
    full_screen = TRUE, fill = TRUE, fillable = TRUE,
    height = NULL,
    #min_height = '125px',
    max_height = '125px',
    ...) {
  
  vb <- function(...){
    value_box(
      theme = value_box_theme(bg = bg, fg = fg),
      showcase_layout = showcase_layout, 
      full_screen = full_screen,
      fill = fill,
      fillable = fillable,
      height = height,
     # min_height = min_height,
      max_height = max_height,
      ... = ...
    )
  }
  tagList(
    #heights_equal = 'row',
    #width = '350px',
    #fixed_width = FALSE,
    vb(
      title = "Average Salary", 
      value = uiOutput(NS(id, 'text_average')),
      showcase = plotly::plotlyOutput(NS(id, 'plot_sparkline_average')), 
      span(textOutput(NS(id, 'text_ave_breakdown')), style = "font-size: 0.8rem")
    ),
    vb(
      title = "Number of Survey Respondents", 
      value = textOutput(NS(id, 'n_respondents')),
      showcase = plotly::plotlyOutput(NS(id, 'plot_sparkline_users')),
    ),
    vb(
      title = "Average Years of Experience", 
      value = textOutput(NS(id, 'text_ave_yoe')),
      showcase = bsicons::bs_icon('clock-history')
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
    
    output$text_ave_breakdown <- renderText({
      if (any(is.na(stats()$value))) { return(HTML('')) }
      .text <- stats() |> 
        filter(name != 'Total') |> pull(.label) |>  paste0(collapse = ', ') 
      HTML(.text)
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