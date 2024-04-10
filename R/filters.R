#' Filters, dropdowns ui
#' @export
filters_ui <- function(id, ...) {
  tagList(
    tags$head(
      #restrict height to 1 line and apply scroll if overflow 
      tags$style(HTML("
      .selectize-input {
        max-height: 200px;
        overflow-y: auto;
      }
      "))
    ),
    tags$style(
      "
      #noborder {
        border: none
        border-top: none
        border-bottom: none
      }
      .accordion, .accordion-item {
        border: none
      }
      "
    ),
    accordion(
      open = NULL,
      id = 'noborder',
      accordion_panel(
        id = 'noborder',
        'Job Title & Country',
        selectizeInput(
          NS(id, "title"), 
          label = NULL, 
          selected = 'Scientist',
          choices = make_grouped_options(salaries, title_category, title_general)
        ),
        selectizeInput(
          NS(id, "location_country"),
          label = NULL,
          selected = 'United States Of America',
          choices = salaries |> pull(location_country) |>  unique() |>  sort()
        )
      ),
      accordion_panel(
        "Precise location",
        p('These responses are not processed.', style = 'color:#f9b928'),
        div(
          style = "margin-bottom:-10px; height:60px; width:100%",
          div(
            style = 'display:block;float:left',
            actionButton(
              NS(id, "select_all"), label = "Select all", style = 'width: 100%'
            )
          ),
          div(
            style = 'display:flex;float:right',
            actionButton(
              NS(id, "deselect_all"), label = "Clear",
              style = 'width:100%'
            )
          )
        ),
        selectizeInput( 
          inputId = NS(id, "location_granular"),
          label = NULL,
          choices = NULL,
          options = list(plugins= list(
            'remove_button'
            # 'clear_button'
          )),
          multiple = TRUE
        )
      )
    ),
    
    radioButtons(
      NS(id, 'date'),
      label = 'Date',
      choices = c('All' , '2024', '2023', '2022'),
      selected = 'All'
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
        filter(
          location_country %in% input$location_country) |>
        pull(location_granular) |> 
        unique() |> 
        sort()
    })
    observe({
      #shinyWidgets::updatePickerInput(
      updateSelectizeInput(
        session = session,
        inputId = 'location_granular',
        choices = .location_granular(),
        selected = .location_granular() 
      )
    })
    observeEvent(input$select_all, {
      updateSelectizeInput(
        session = session,
        inputId = 'location_granular',
        choices = .location_granular(),
        selected = .location_granular()
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
      
      .out <- list(
        .date = .date,
        title = input$title,
        location_country = input$location_country,
        location_granular = .location_granular()
      )
      
      return(.out)
    })
    
  })
}