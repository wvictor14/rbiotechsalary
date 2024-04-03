#' Filters, dropdowns ui
#' @export
filters_ui <- function(id, ...) {
  tagList(
    shiny.fluent::Dropdown.shinyInput(
      NS(id, 'title'),
      multiSelect = TRUE,
      value = 'Scientist',
      label = NULL,
      options = salaries |> make_grouped_options(title_category, title_general)
    ),
    shiny.fluent::Dropdown.shinyInput(
      NS(id, "location_country"),
      placeHolder = "Select location",
      multiSelect = FALSE,
      value = 'United States Of America',
      options =  salaries |>
        mutate(key = location_country, text = location_country) |>
        select(key, text) |>
        distinct()  |>
        arrange(key)
    ),
    shiny.fluent::Dropdown.shinyInput(
      inputId = NS(id, "location_granular"),
      placeHolder = "Select sub-location",
      multiSelect = TRUE
    ),
    
    shiny.fluent::Stack(
      horizontal = TRUE,
      shiny.fluent::DefaultButton.shinyInput(
        NS(id, "select_all"), text = "Select all"
      ),
      shiny.fluent::DefaultButton.shinyInput(
        NS(id, "deselect_all"), text = "Deselect all"
      )
    ),
    shiny.fluent::Stack(
      horizontal = TRUE,
      tokens = list(childrenGap = 10),
      shiny.fluent::ChoiceGroup.shinyInput(
        NS(id, 'date'),
        options = c('All' , '2024', '2023', '2022') %>%
          tibble(key = ., text = .),
        styles = list(flexContainer = list(display = "flex", gap = 10)),
        value = 'All')
    )
  )
}

#' filter server
#' @export
filters_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # location input reactives
    .location_granular <- eventReactive(input$location_country, {
      salaries |>
        filter(location_country %in% input$location_country) |>
        mutate(key = location_granular, text = location_granular ) |>
        select(key, text) |>
        distinct()  |>
        arrange(key) |>
        mutate(across(everything(), as.character))
    })
    
    
    observe({
      shiny.fluent::updateDropdown.shinyInput(
        session = session,
        inputId = 'location_granular',
        multiSelect = TRUE,
        value = .location_granular() |>  pull(key),
        options =  .location_granular()
      )
    })
    observeEvent(input$deselect_all, {
      shiny.fluent::updateDropdown.shinyInput(
        session = session,
        inputId = 'location_granular',
        multiSelect = TRUE,
        value = NULL,
        options =  .location_granular()
      )
    })
    observeEvent(input$select_all, {
      shiny.fluent::updateDropdown.shinyInput(
        session = session,
        inputId = 'location_granular',
        multiSelect = TRUE,
        value = .location_granular() |>  pull(key),
        options =  .location_granular()
      )
    })
    
    # return filtered salary data
    reactive({
      req(input$title)
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
          location_country %in% input$location_country,
          location_granular %in% input$location_granular
        )
    })
    
  })
}