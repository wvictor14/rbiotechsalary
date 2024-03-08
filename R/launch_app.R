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
  shiny.fluent::fluentPage(
    shiny.fluent::Text(variant = "xxLarge", "r/biotech Salary Tracker is live!")
  )

  uiOutput("analysis")
}

#' app server
#' @param input input
#' @param output output
#' @param session session
#' @export
rb_server <- function(input, output, session) {

  .salaries <- reactive({
    salaries |>
      select(timestamp:day, salary_base:bonus, everything())
  })

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
      tokens = list(childrenGap = 5),
      shiny.fluent::Text(variant = "large", "Salary details", block = TRUE),
      div(style="max-height: 500px; overflow: auto", items_list)
    )
  })
}
