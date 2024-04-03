#' new ui
#' @export
#' @examples \dontrun{
#'   launch_app(ui = rb_ui2, server = function(input, output) {})
#'   launch_app(ui = rb_ui2, server = rb_server_2)
#'   
#' }
rb_ui2 <- function() {
  link_github <- tags$a(
    shiny::icon("github"), "source",
    href = "https://github.com/wvictor14/rbiotechsalary",
    target = "_blank"
  )
  
  page_navbar(
    title = "My App",
    nav_panel(
      title = "One", 
      p("First page content."),
      filters_ui('filters'),
      table_salary_stats_ui('table_salary_stats'),
      
      plotly::plotlyOutput("plot"),
      
      gt::gt_output('table_career_progression'),
      plotly::plotlyOutput('plot_experience'),
      table_raw_ui("table_raw")
    ),
    nav_panel(title = "Two", p("Second page content.")),
    nav_spacer(),
    nav_menu(
      title = "Links",
      align = "right",
      nav_item(link_github)
    )
  )
}

#' updated server function
#' @export
rb_server_2 <- function(input, output, session) {
  
  .salaries <- filters_server('filters')
  table_salary_stats_server('table_salary_stats', .salaries)
  
  
  output$plot <- plotly::renderPlotly({
    plot_salary(.salaries(), title = 'Total Compensation (Base + Bonus)')
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
      p <- ggplot() + theme_minimal(); plotly::ggplotly(p, height = 250)
    }
  })
  
  # render the table + other components
  table_raw_server('table_raw', .salaries)
  
  
  
}