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
  link_github <- tags$a(
    shiny::icon("github"), "source",
    href = "https://github.com/wvictor14/rbiotechsalary",
    target = "_blank"
  )
  .colors <- list(
    'primary' = '#41AB5DFF'
  )
  page_navbar(
    theme = bs_theme(
      version = 5, 
      fg = '#1D262E',
      bg = '#EEE8D5',
      primary = .colors$primary
    ),
    title = "r/biotech salary",
    fillable = FALSE,
    
    sidebar = sidebar(
      width = 300,
      p('Choose your role'),
      filters_ui('filters')
    ),
    
    ### panel 1 ----
    nav_panel(
      title = "Salaries",
      h1(textOutput('content_title')),
      layout_columns(value_boxes_stats_ui('value_boxes')),
      plotly::plotlyOutput("plot_salary_histogram"),
      card(
        full_screen = TRUE,
        min_height = '400px',
        card_body(
          table_raw_ui("table_raw")
        )
      )
    ),
    
    ### panel 2 ----
    nav_panel(
      title = "Career progression", 
      card(
        full_screen = TRUE,
        card_body(
          min_height = '300px',
          layout_columns(
            col_widths = c(4, 8),
            gt::gt_output('table_career_progression'),
            plotly::plotlyOutput('plot_experience')
          )
        )
      )
    ),
    nav_spacer(),
    nav_menu(
      title = "Links",
      align = "right",
      nav_item(link_github)
    ),
    nav_item(
      input_dark_mode(id = "dark_mode", mode = "dark")
    )
  )
  
}
