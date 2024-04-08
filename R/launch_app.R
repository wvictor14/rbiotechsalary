#' launch rbiotechsalary app
#' 
#' @export
#' @import shiny bslib dplyr
#' @examples \dontrun{
#' 
#'  launch_app()
#'   
#'  # launch ui without server
#'  launch_app(server = function(input, output) {})
#'  
#' }
launch_app <- function(options = list(), ui = rb_ui, server = rb_server, ...) {
  
  data(salaries)
  message('salaries loaded')
  
  version <- paste0('v', as.character(utils::packageVersion('rbiotechsalary')))
  #shiny::addResourcePath("www", "www")
  message('Running rbiotechsalary version ', version)
  
  shiny::shinyApp(ui, server,  options = list(launch.browser = TRUE), ...)
}