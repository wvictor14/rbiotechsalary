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
      value_box(
        title = 'Number of Salaries',
        value = shiny::textOutput(ns('number_of_salaries'))
      ),
      value_box(
        title = 'Public or Private',
        value = shiny::htmlOutput(ns('company_public_or_private'))
      ),
      value_box(
        title = 'Company Size',
        value = shiny::textOutput(ns('company_size'))
      )
    ),
    bslib::layout_column_wrap(
      bslib::card(
        class = 'border border-secondary p-0',
        bslib::card_title(
          shiny::textOutput(ns('company_reviews_title'))
        ),
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

    output$company_reviews_title <- renderText({
      glue::glue("Browse through company reviews for {input$select_company}")
    })

    output$company_reviews <- reactable::renderReactable({
      companies_selected() |>
        select(date, company_review, title_category) |>
        filter(!is.na(company_review)) |>
        reactable_rbs()
    })

    # value boxes ----
    output$number_of_salaries <- renderText({
      nrow(companies_selected())
    })
    output$company_size <- renderText({
      companies_selected() |>
        count(company_detail_approximate_company_size) |>
        arrange(desc(n)) |>
        slice(1) |>
        pull(company_detail_approximate_company_size)
    })
    output$company_public_or_private <- renderUI({
      npub <- sum(
        companies_selected()$public_or_private == 'Public',
        na.rm = TRUE
      )
      npriv <- sum(
        companies_selected()$public_or_private == 'Private',
        na.rm = TRUE
      )

      majority_vote <- ifelse(npub >= npriv, 'Public', 'Private')

      p <- scales::percent(npub / (npub + npriv))
      shiny::HTML(
        glue::glue(
          "<span>
            {p} employees describe <b>{input$select_company}</b> 
            as a <b>{majority_vote}</b> company
          </span>"
        )
      )
    })
  })
}
