#' Filters, dropdowns ui
#' @export
filters_ui <- function(id, ...) {
  tagList(
    tags$head(
      # Note the wrapping of the string in HTML()
      tags$style(HTML("
      .selectize-input {
        max-height: 200px;
        overflow-y: auto;
      }
      "))),
    selectizeInput(
      NS(id, "title"), 
      label = NULL, 
      choices = make_grouped_options(salaries, title_category, title_general) 
    ),
    selectizeInput(
      NS(id, "location_country"), 
      label = NULL, 
      selected = 'United States Of America',
      choices = salaries |> pull(location_country) |>  unique() |>  sort()
    ),
    #shinyWidgets::pickerInput(
    selectizeInput( 
      inputId = NS(id, "location_granular"),
      label = NULL,
      choices = NULL,
      options = list(plugins= list(
        #'remove_button', 
        'clear_button'
        )),
      #options = list(
      #  `actions-box` = TRUE), 
      multiple = TRUE
    ),
    
    actionButton(
      NS(id, "select_all"), label = "Select all"
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
      #shinyWidgets::updatePickerInput(
      updateSelectizeInput(
        session = session,
        inputId = 'location_granular',
        choices = .location_granular() |>  pull(key),
        selected = .location_granular() |>  pull(key) 
      )
    })
    observeEvent(input$select_all, {
      updateSelectizeInput(
        session = session,
        inputId = 'location_granular',
        choices = .location_granular() |>  pull(key),
        selected = .location_granular() |>  pull(key)
      )
    })
    
    # return filtered salary data
    reactive({
      req(.location_granular())
      req(input$location_country)
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
          location_country %in% input$location_country,
          location_granular %in% input$location_granular
        )
    })
    
  })
}