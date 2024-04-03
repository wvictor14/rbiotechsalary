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
      
      gt::gt_output('salary_stats_text')
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
  
  # table salary stats
  output$salary_stats_text <- gt::render_gt({
    if (nrow(.salaries()) > 0) {
      gt_salary_stats(.salaries())
    } else {
      tibble('No matching salary data' = '') |> gt::gt() |>
        gt::tab_options(
          table.border.top.style = "hidden",
          table.border.bottom.style = "hidden")
    }
  })
}