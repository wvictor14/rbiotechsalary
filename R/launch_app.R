#' launch rbiotechsalary app
#' 
#' @param server when not supplied, will use rb_server
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
launch_app <- function(options = list(), ui = rb_ui, server = NULL, ...) {
  
  # load data for app
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
  
  # set up server environment
  if (is.null(server)) {
    
    # assign variables to server environment
    source('R/server.R', local = TRUE)
    server_env <- environment(rb_server)
    
    server_env$salaries <- salaries
    server_env$skim_raw_data <- skim_raw_data
  } else {
    rb_server <- server
  }
  
  # run app
  message('Running rbiotechsalary version ', version)
  shiny::shinyApp(ui, rb_server, options = list(launch.browser = TRUE), ...)
}