#' Filters, dropdowns ui
#' @export
#' @examples \dontrun{
#'
#' }
filters_ui <- function(id, .salaries, ...) {
  tagList(
    tags$head(
      #restrict height to 1 line and apply scroll if overflow 
      tags$style(HTML("
      "))
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
          choices = make_grouped_options(.df = .salaries, title_category, title_general)
        ),
        selectizeInput(
          NS(id, "location_country"),
          label = NULL,
          selected = 'United States Of America',
          choices = .salaries |> pull(location_country) |>  unique() |>  sort()
        )
      ),
      accordion_panel(
        "Filter by precise location",
        p('Data not normalized/processed.', style = 'color:#f9b928'),
        selectizeInput( 
          inputId = NS(id, "location_granular"),
          label = NULL,
          choices = NULL,
          options = list(
            placeholder = 'All selected',
            plugins= list(
              'remove_button', 'auto_position'
              # 'clear_button'
            )),
          multiple = TRUE
        ),
        actionButton(
          NS(id, "deselect_all"), label = "Clear selection",
          style = '
            color: grey;
            height:35px; width: fit-content; 
            padding-top:0px;padding-bottom:0px;
            padding-left: 10px; padding-right: 16px;
            float: left; 
            align: left; 
          ', 
          class = 'btn btn-dark'#btn-outline-info',
        )
      )
    ),
    selectizeInput(
      NS(id, 'education'),
      label = 'Education',
      selected = c('PhD', 'Masters', 'Bachelors'),
      choices = .salaries$experience_highest_degree |>  unique() |>  sort(na.last = TRUE),
      multiple = TRUE,
      options = list(
        placeholder = 'All selected',
        plugins= list(
          'remove_button', 'auto_position', 'clear_button'
        )
      )
    )
  )
}

#' filter server
#' @export
filters_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # location input reactives
    .location_granular_choices <- eventReactive(input$location_country, {
      gran <- salaries |>
        filter(
          location_country %in% input$location_country) |>
        pull(location_granular) |> 
        unique() |> 
        sort()
    })
    observe({
      updateSelectizeInput(
        session = session,
        inputId = 'location_granular',
        choices = .location_granular_choices(),
        selected = NULL 
      )
    })
    observeEvent(input$deselect_all, {
      updateSelectizeInput(
        session = session,
        inputId = 'location_granular',
        choices = .location_granular_choices(),
        selected = NULL
      )
    })
    
    # return filtered salary data
    reactive({
      req(input$location_country)
      
      .out <- list(
        title = input$title,
        location_country = input$location_country,
        location_granular = input$location_granular,
        education = input$education
      )
      
      return(.out)
    })
    
  })
}