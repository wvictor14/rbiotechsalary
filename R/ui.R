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
      fg = '#EEE8D5',
      bg =  '#232627',
      primary = .colors$primary,
      info = '#EEE8D5',
      "border-color" = 'rgba(255,255,255,0.1)', #495057',
      "border-color-translucent" = 'rgba(255,255,255,0.1)', #495057'
      "accordion-border-width" = "1px"
    ) ,
    title = "r/biotech salaries",
    footer = tags$footer(
      style = "padding: 0px; text-align: center; position: fixed; bottom: 0; width: 100%;",
      p(
        style = "margin: 0; color: #888;",
        glue::glue("rbiotechsalary {version}")
      )
    ),
    
    sidebar = sidebar(
      title = NULL,
      gap = '1px',
      width = 300,
      filters_ui('filters')
    ),
    
    ### panel 1 ----
    nav_panel(
      title = "Salaries",
      htmltools::h1(shiny::textOutput('content_title_1')),
      layout_column_wrap(
        
        style = htmltools::css(grid_template_columns = "2fr 1fr"),
        plotly::plotlyOutput("plot_salary_histogram"),
        value_boxes_stats_ui('value_boxes')
      )
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
      #input_dark_mode(id = "dark_mode", mode = "dark")
    )
  )
  
}
