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
launch_app <- function(...) {
  
  # load data for app
  skim_raw_data <- load_raw_data() |> 
    skimr::skim() |> 
    as_tibble() |> 
    rename_with(~stringr::str_remove_all(.x, 'character\\.')) |> 
    select(-skim_type, -whitespace) 
  
  message('data summarized with skimr')
  
  salaries <- readr::read_csv(
    here::here('data', 'salary_results_cleaned.csv'),
    show_col_types = FALSE
  ) |> 
    rb_relevel()
  message('processed data loaded')
  
  version <- paste0('v', as.character(utils::packageVersion('rbiotechsalary')))
  #shiny::addResourcePath("www", "www")
  
  # source server.R in this environment to gain access to above variables
  source(here::here('R', 'server.R'), local = TRUE)
  source(here::here('R', 'ui.R'), local = TRUE)
  
  # run app
  message('Running rbiotechsalary version ', version)
  shiny::shinyApp(
    rb_ui(salaries), 
    rb_server, 
    options = list(launch.browser = TRUE), 
    ...
  )
}