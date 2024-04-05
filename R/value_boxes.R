value_boxes_stats_ui <- function(id) {
  layout_columns(
    value_box(
      title = "Average Salary", 
      value = uiOutput(NS(id, 'text_average')),
      theme= 'text-success',
      #theme = value_box_theme(bg = "#287A28", fg = "#FFFFFF"),
      showcase = plotly::plotlyOutput(NS(id, 'plot_sparkline_average')), 
      showcase_layout = "left center", 
      full_screen = TRUE,
      fill = TRUE, 
      height = NULL,
      uiOutput(NS(id, 'text_ave_breakdown'))
    ),
    value_box(
      title = "Number of Survey Respondents", 
      value = p('631'),
      theme= 'text-success',
      #theme = value_box_theme(bg = "#287A28", fg = "#FFFFFF"),
      showcase = bsicons::bs_icon('person-lines-fill'),
      showcase_layout = "left center", 
      full_screen = TRUE,
      fill = TRUE, 
      height = NULL
    ),
    value_box(
      title = "Placeholder", 
      value = p('Sixty-one'),
      theme= 'text-success',
      #theme = value_box_theme(bg = "#287A28", fg = "#FFFFFF"),
      showcase = bsicons::bs_icon('currency-exchange'),
      showcase_layout = "left center", 
      full_screen = TRUE,
      fill = TRUE, 
      height = NULL
    ),
  )
}
value_boxes_stats_server <- function(id, .salaries) {
  stopifnot(is.reactive(.salaries))
  moduleServer(id, function(input, output, session) {
    
    # value boxes
    output$plot_sparkline_average <- plotly::renderPlotly(
      .salaries() |>
        
        # take most recent 100 submissions for sparkline
        arrange(desc(date)) |> 
        slice(1:100) |> 
        plot_sparkline(color = '#41AB5DFF')
    )
    
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
      HTML(paste0(.text, "<br>"))
    })
    
    output$text_ave_breakdown <- renderUI({
      HTML(
        stats() |> filter(name != 'Total') |> pull(.label) |>  paste0(collapse = '<br>') 
      )
    })
  })
}