#' launch rbiotechsalary app
#' 
#' @export
#' @import shiny bslib dplyr
#' @importFrom reactable colDef reactable
#' @examples \dontrun{
#' 
#'  launch_app()
#'   
#'  # launch ui without server
#'  launch_app(server = function(input, output) {})
#'  
#' }
launch_app <- function(options = list(), ui = rb_ui, server = rb_server, ...) {
  
  skim_raw_data <- load_raw_data() |> 
    skimr::skim() |> 
    as_tibble() |> 
    rename_with(~stringr::str_remove_all(.x, 'character\\.')) |> 
    select(-skim_type, -whitespace) 
  
  message('data summarized with skimr')
  
  data(salaries)
  message('processed data loaded')
  
  version <- paste0('v', as.character(utils::packageVersion('rbiotechsalary')))
  #shiny::addResourcePath("www", "www")
  message('Running rbiotechsalary version ', version)

  # assign variables to server environment
  source('R/server.R', local = TRUE)
  server_env <- environment(rb_server)
  
  # variables
  server_env$salaries <- salaries
  server_env$skim_raw_data <- skim_raw_data
  
  shiny::shinyApp(rb_ui, rb_server, options = list(launch.browser = TRUE), ...)
}