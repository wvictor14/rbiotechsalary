#' @export
#' @import shiny shiny.fluent
launch_app <- function(..., salaries = NULL) {

  if (is.null(data)) {
    data(salaries)
    salaries |>  glimpse()
    message('salaries loaded')
  }

  version <- paste0('v', as.character(utils::packageVersion('r_biotech_salary')))
  #shiny::addResourcePath("www", "www")

  rb_ui <- rb_ui()
  rb_server <- rb_server

  shiny::shinyApp(rb_ui, rb_server, options = list(launch.browser = TRUE), ...)
}

#' app ui
#'
#' @export
rb_ui <- function() {

  filters <- tagList(
    shiny.fluent::Dropdown.shinyInput(
      'title',
      multiSelect = TRUE,
      value = 'Scientist',
      label = NULL,
      options = salaries |>
        mutate(key = title_general, text = title_general) |>
        select(key, text) |>
        distinct()  |>
        arrange(key) |>
        mutate(across(everything(), ~ifelse(is.na(.x), '(Missing)', .x)))
    ),
    shiny.fluent::Dropdown.shinyInput(
      "location",
      placeHolder = "Select location",
      multiSelect = FALSE,
      value = 'United States Of America',
      options =  salaries |>
        mutate(key = location_country, text = location_country) |>
        select(key, text) |>
        distinct()  |>
        arrange(key) |>
        mutate(across(everything(), ~ifelse(is.na(.x), '(Missing)', .x)))
    ),
    shiny.fluent::Stack(
      horizontal = TRUE,
      tokens = list(childrenGap = 10),
      shiny.fluent::ChoiceGroup.shinyInput(
        'date',
        options = c('All' , '2024', '2023', '2022') %>%
          tibble(key = ., text = .),
        styles = list(flexContainer = list(display = "flex", gap = 10)),
        value = 'All')
    )
  )

  shiny.fluent::fluentPage(
    shiny.fluent::Text(variant = "xxLarge", "r/biotech Salary Tracker is live!"),
    tags$style(".card { padding: 28px; margin-bottom: 28px; }"),

    shiny.fluent::Stack(
      tokens = list(childrenGap = 10), horizontal = TRUE,
      makeCard(content = shiny.fluent::Stack(
        filters,
        gt::gt_output('salary_stats_text')
      ),
      size = 3,
      style = "max-height: 250px;"),
      makeCard(content = shiny.fluent::Stack(
        plotly::plotlyOutput("plot")
      ),
      size = 9,
      style = "max-height: 250px")
    ),
    shiny.fluent::Stack(
      tokens = list(childrenGap = 10), horizontal = TRUE,
      makeCard(
        content = div(
          style="max-height: 250px; overflow: auto",
          DT::dataTableOutput("table_raw", height = '250px')
        ),
        size = 5,
        style = "max-height: 250px"
      ),
      makeCard(
        content = plotly::plotlyOutput('plot_experience'),
        size = 7,
        style = "max-height: 250px"
      )
    )
  )
}

#' app server
#' @param input input
#' @param output output
#' @param session session
#' @export
#' @import dplyr ggplot2
rb_server <- function(input, output, session) {

  .salaries <- reactive({
    req(input$date)
    .date <- switch(
      input$date,
      'All' = c('2024', '2023', '2022'),
      '2024' = '2024',
      '2023' = '2023',
      '2022' = '2022'
    )

    salaries |>
      filter(
        lubridate::year(date) %in% .date,
        title_general %in% input$title,
        location_country %in% input$location
      )
  })

  #plot
  output$salary_stats_text <- gt::render_gt({
    if (nrow(.salaries() > 0)) {
      gt_salary_stats(.salaries())
    } else {
      tibble('No matching salary data' = '') |> gt::gt() |>
        gt::tab_options(
          table.border.top.style = "hidden",
          table.border.bottom.style = "hidden")
    }
  })
  output$plot <- plotly::renderPlotly({
    plot_salary(.salaries(), title = 'Total Compensation (Base + Bonus)')
  })

  output$plot_experience <- plotly::renderPlotly({

    # suppress warnings for app session
    storeWarn<- getOption("warn")
    options(warn = -1)

    if (nrow(.salaries()) > 0) {
      plot_experience(.salaries())
    } else {
      p <- ggplot() + theme_minimal(); plotly::ggplotly(p, height = 250)
    }
  })

  # render the table + other components
  output$table_raw <- DT::renderDataTable(
    server = TRUE, expr = {
      items_list <- if(nrow(.salaries()) > 0){
        selected_cols <- .salaries() |>
          arrange(desc(date)) |>
          select(
            `Job title` = title_general,
            `Location` = location_country,
            `Salary (base)` = salary_base,
            `Bonus %` = bonus_pct,
            `Date` = date
          ) |>
          DT::datatable(
            rownames = FALSE,
            options = list('dom' = 'ftipr')) |>
          DT::formatCurrency('Salary (base)') |>
          DT::formatPercentage('Bonus %') |>
          DT::formatDate('Date', method = 'toLocaleDateString')
        selected_cols
      } else {
        tibble() |>  DT::datatable()
      }
      return(items_list)
    })

}

#' @export
makeCard <- function(title = NULL, content, size = 12, style = "") {


  div(
    class = glue::glue("card ms-depth-8 ms-sm{size} ms-xl{size}"),
    style = style,
    Stack(
      tokens = list(childrenGap = 5),
      Text(variant = "large", title, block = TRUE),
      content
    )
  )
}
