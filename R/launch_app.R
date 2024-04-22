#' launch rbiotechsalary app
#' 
#' @param server when not supplied, will use rb_server
#' @export
#' @import shiny bslib dplyr
#' @importFrom reactable colDef reactable
#' @examples \dontrun{
#'  
#' }
launch_app <- function(launch.browser = TRUE, ...) {
  withr::with_dir(
    shiny::runApp(here::here('R'), launch.browser = launch.browser, ...)
  )
}