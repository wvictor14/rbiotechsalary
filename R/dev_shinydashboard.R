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
      table_salary_stats_ui('table_salary_stats')
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
  
}