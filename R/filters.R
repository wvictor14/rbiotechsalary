#' Filters, dropdowns ui
#' @export
#' @examples \dontrun {
#' 
#'   ui <- page_sidebar(theme = bs_theme(
#'       version = 5,  fg = '#EEE8D5', bg =  '#232627', primary = 'seagreen'),
#'     #actionButton('poo', 'poo', class = 'btn-secondary'),
#'     #selectInput('select', 'select', choices = '1')
#'     filters_ui('poo')
#'   )
#'   
#'  shinyApp(ui = ui, server = function(input, output) {})
#'
#' }
filters_ui <- function(id, ...) {
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
          style = 'height:40px; width:100%;padding-top:0px;padding-bottom:0px', 
          class = 'btn btn-dark'#btn-outline-info',
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
        selected = NULL 
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
    observeEvent(input$deselect_all, {
      updateSelectizeInput(
        session = session,
        inputId = 'location_granular',
        choices = .location_granular(),
        selected = NULL
      )
    })
    
    # if none is selected, return all
    location_granular_selected <- eventReactive(input$location_granular, {
      if (is.null(input$location_granular)) {
        .loc_gran <- .location_granular()
      } else {
        .loc_gran <- input$location_granular
      }
      
      return(.loc_gran)
    }, ignoreNULL = FALSE)
    
    # return filtered salary data
    reactive({
      req(location_granular_selected())
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
        location_granular = location_granular_selected()
      )
      
      return(.out)
    })
    
  })
}