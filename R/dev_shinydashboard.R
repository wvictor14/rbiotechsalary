#' new ui
#' @export
#' @examples \dontrun{
#'   launch_app(ui = rb_ui2, server = function(input, output) {})
#'   launch_app(ui = rb_ui2, server = rb_server_2)
#' }
rb_ui2 <- function() {
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
      #presets = 'shiny',
      fg = '#161C21',
      bg = '#EEE8D5',
      'primary' = .colors$primary
    ),
    title = "r/biotech salary",
    fillable = FALSE,
    sidebar = sidebar(
      p('Choose your role'),
      filters_ui('filters')
    ),
    nav_panel(
      title = "Salaries", 
      
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

#' updated server function
#' @export
rb_server_2 <- function(input, output, session) {
  
  .salaries <- filters_server('filters')
  
  table_salary_stats_server('table_salary_stats', .salaries)
  value_boxes_stats_server('value_boxes', .salaries)
  
  output$plot_salary_histogram <- plotly::renderPlotly({
    if (nrow(.salaries()) == 0 ) return(NULL)
    
    plot_salary_histogram(.salaries(), salary_total)
  })
  
  
  output$table_career_progression <- gt::render_gt({
    gt_career_progression(.salaries())
  })
  output$plot_experience <- plotly::renderPlotly({
    
    # suppress warnings for app session
    storeWarn<- getOption("warn")
    options(warn = -1)
    
    if (nrow(.salaries()) > 0) {
      plot_experience(.salaries())
    } else {
      p <- ggplot() + theme_minimal()
      plotly::ggplotly(p)
    }
  })
  
  # render the table + other components
  table_raw_server('table_raw', .salaries, height = gt::px(400))
  
}