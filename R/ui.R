#' @describeIn launch_app UI components
#' @export
#' @examples \dontrun{
#' 
#'   # launch with just ui
#'   launch_app(ui = rb_ui, server = function(input, output) {})
#' 
#'   # launch with server
#'   launch_app(ui = rb_ui, server = rb_server)
#' }
rb_ui <- function() {
  
  
  version <- paste0('v', as.character(utils::packageVersion('rbiotechsalary')))
  link_add_data <- shiny::tags$a(
    bsicons::bs_icon("plus-lg"), " Add Your Salary",
    href = "https://docs.google.com/forms/d/e/1FAIpQLSeXpPvkjdf8EPRU_cAdyMAUFVjgx67nyVgDxV7TNHeFkR2k9A/viewform",
    target = "_blank"
  )
  link_github <- shiny::tags$a(
    shiny::icon("github"), " Source",
    href = "https://github.com/wvictor14/rbiotechsalary",
    target = "_blank"
  )
  link_reddit <- shiny::tags$a(
    bsicons::bs_icon("reddit"), " Reddit",
    href = "https://www.reddit.com/r/biotech/comments/125cy06/rbiotech_salary_and_company_survey/",
    target = "_blank"
  )
  link_google <- shiny::tags$a(
    bsicons::bs_icon("file-earmark-spreadsheet"), " Raw Data",
    href = "https://docs.google.com/spreadsheets/d/1G0FmJhkOME_sv66hWmhnZS5qR2KMTY7nzkxksv46bfk/edit#gid=491268892",
    target = "_blank"
  )
  
  
  .colors <- list(
    'primary' = '#41AB5DFF'
  )
  page_navbar(
    theme = bs_theme(
      version = 5, 
      fg = '#0c1014', #'#1D262E',
      bg = '#EEE8D5',
      primary = .colors$primary,
      #"border-width" = "none"
    ) ,
    title = "r/biotech salaries",
    footer = tags$footer(
      style = "padding: 0px; text-align: center; position: fixed; bottom: 0; width: 100%;",
      
      # css
      tags$style(
        "
      #noborder {
        border: none;
        border-top: none;
        border-bottom: none
      }
      .accordion, .accordion-item {
        border: none;
        border-top: none
      }
    
      .h1, h1{color: #EEE8D5;}
      "
      ),
      p(
        style = "margin: 0; color: #888;",
        glue::glue("rbiotechsalary {version}")
      )
    ),
    
    sidebar = sidebar(
      title = 'Filter salaries by',
      gap = '1px',
      width = 300,
      filters_ui('filters')
    ),
    
    ### panel 1 ----
    nav_panel(
      title = "Salaries",
      htmltools::h1(shiny::textOutput('content_title_1')),
      layout_columns(value_boxes_stats_ui('value_boxes')),
      plotly::plotlyOutput("plot_salary_histogram"),
    ),
    
    nav_panel(
      title = 'Raw data',
      htmltools::h1(shiny::textOutput('content_title_2')),
      table_raw_ui("table_raw")
    ),
    ### panel career progression ----
    nav_panel(
      title = "Career progression", 
      htmltools::h1('Average Total Compensation by Years of Experience'),
      card(
        full_screen = TRUE,
        card_body(
          min_height = '300px',
          plotly::plotlyOutput('plot_career_progression')
        )
      )
    ),
    nav_spacer(),
    nav_menu(
      title = "Links",
      align = "right",
      nav_item(link_add_data),
      nav_item(link_google),
      nav_item(link_reddit),
      nav_item(link_github)
      
    ),
    nav_item(
      input_dark_mode(id = "dark_mode", mode = "dark")
    )
  )
  
}
