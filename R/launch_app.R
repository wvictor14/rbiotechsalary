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
    ComboBox.shinyInput('title', value = NULL, options = NULL, label = 'Job Title'),
    Text(variant = 'xLarge', 'Survey response date:'),
    DatePicker.shinyInput("fromDate", value = as.Date('2024/01/01'), label = "From date"),
    DatePicker.shinyInput("toDate", value = Sys.time() |> lubridate::date(), label = "To date")
  )

  shiny.fluent::fluentPage(
    shiny.fluent::Text(variant = "xxLarge", "r/biotech Salary Tracker is live!"),
    tags$style(".card { padding: 28px; margin-bottom: 28px; }"),

    shiny.fluent::Stack(
      tokens = list(childrenGap = 10), horizontal = TRUE,
      makeCard("", filters, size = 4, style = "max-height: 320px;"),
      makeCard(
        "Salary histogram",
        plotly::plotlyOutput("plot"),
        size = 8,
        style = "max-height: 320px")
    ),
    uiOutput("analysis")
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
      select(timestamp:day, salary_base:bonus, everything()) |>
      filter(
        timestamp >= input$fromDate,
        timestamp <= input$toDate,
        role_title_of_current_position %in% input$title
      )
  })

  # filters
  observe({
    choices <- salaries$role_title_of_current_position |>  unique()
    options <- tibble(key = choices, text = choices)
    updateComboBox.shinyInput(
      session,
      'title',
      value = NULL,
      options = options
    )
  })

  #plot
  output$plot <- plotly::renderPlotly({
    p <- ggplot(.salaries(), aes(
      x = salary_base, fill = role_title_of_current_position)) +
      geom_histogram()  +
      scale_fill_discrete(guide ='none') +
      theme_light() +
      scale_x_continuous(labels = scales::number)
    plotly::ggplotly(p, height = 300)
  })

  # render the table + other components
  output$analysis <- renderUI({
    #browser()
    items_list <- if(nrow(.salaries()) > 0){
      shiny.fluent::DetailsList(
        items = .salaries(),
        columns = tibble(
          fieldName = colnames(.salaries()),
          name =  colnames(.salaries()),
          key = fieldName
          )
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
makeCard <- function(title, content, size = 12, style = "") {
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
