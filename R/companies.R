#' Filters, dropdowns ui
#' @export
#' @examples \dontrun{
#'
#' }
companies_ui <- function(id, choices, ...) {
  ns <- NS(id)
  tagList(
    selectizeInput(
      ns('select_company'),
      label = 'Company',
      choices = choices
    ),

    bslib::layout_column_wrap(
      bslib::card(
        bslib::card_title('Reviews'),
        bslib::card_body(
          reactable::reactableOutput(ns('company_reviews'))
        )
      )
    )
  )
}

#' Companies server
#' @export
companies_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    companies_selected <- reactive({
      companies |>
        filter(
          company_or_institution_name == input$select_company
        )
    })

    output$company_reviews <- reactable::renderReactable({
      companies_selected() |>
        select(company_review) |>
        filter(!is.na(company_review)) |>
        reactable_rbs()
    })
  })
}
