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
      label = 'Job Title',
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
      multiSelect = TRUE,
      value = salaries$location_country |> unique() |>  sort(),
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
      shiny.fluent::DatePicker.shinyInput(
        "fromDate", value = as.Date('2022/01/01'), label = "From date"),
      shiny.fluent::DatePicker.shinyInput(
        "toDate", value = Sys.time() |> lubridate::date(), label = "To date")
    )
  )

  shiny.fluent::fluentPage(
    shiny.fluent::Text(variant = "xxLarge", "r/biotech Salary Tracker is live!"),
    tags$style(".card { padding: 28px; margin-bottom: 28px; }"),

    shiny.fluent::Stack(
      tokens = list(childrenGap = 10), horizontal = TRUE,
      makeCard(content = filters, size = 4, style = "max-height: 360px;"),
      makeCard(content = shiny.fluent::Stack(
          shiny::uiOutput('salary_stats_text'),
          plotly::plotlyOutput("plot")
        ),
        size = 8,
        style = "max-height: 360px")
    ),
    shiny::uiOutput("analysis")
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
    req(input$fromDate)
    salaries |>
      filter(
        date >= input$fromDate,
        date <= input$toDate,
        title_general %in% input$title,
        location_country %in% input$location
      )
  })

  #plot
  output$salary_stats_text <- shiny::renderUI({
    plot_salary_title(.salaries()) |>
      stringr::str_replace_all('\\\n', '<br/>') |>
      shiny::HTML()
  })
  output$plot <- plotly::renderPlotly({
    p <- plot_salary(.salaries())
    p$x$layout$title <- NULL # remove title
    p
    })

  # render the table + other components
  output$analysis <- renderUI({
    #browser()
    items_list <- if(nrow(.salaries()) > 0){
      selected_cols <- .salaries() |>
        select(
          title_general, title_detail, location_country, salary_base,
          bonus_pct, date,
        ) |>
        mutate(
          date = as.character(date),
          salary_base = scales::dollar(salary_base),
          bonus_pct = scales::percent(bonus_pct, accuracy = 1))

      shiny.fluent::DetailsList(
        items = selected_cols,
        columns = tibble(
          fieldName = colnames(selected_cols),
          name =  colnames(selected_cols)
        ),
        constrainMode = 0
      )
    } else {
      p("No matching salary data.")
    }

    shiny.fluent::Stack(
      tokens = list(childrenGap = 10), horizontal = TRUE,
      makeCard("Salaries data", div(style="max-height: 500px; overflow: auto", items_list))
    )
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
