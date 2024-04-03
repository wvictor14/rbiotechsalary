#' new ui
#' @export
#' @examples \dontrun{
#'   launch_app(ui = rb_ui2, server = function(input, output) {})
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
      p("First page content.")
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