
#' @export
load_raw_data <- function(years = c('2022', '2023', '2024')) {
  
  files <- function(x) {
    fs::path_package(
      'rbiotechsalary', 'extdata', glue::glue('{x}_survey_results.csv')
    )
  }
  
  years %>% 
    stats::setNames(., nm = .) |>   
    purrr::map(
      \(x) readr::read_csv(
        files(x),
        col_types = readr::cols(.default = "c")
      )
    ) |> 
    purrr::list_rbind(names_to = 'sheet_year')
}

#' @export
#' @examples 
#' 
#'  load_raw_data() |> 
#'   skim_data_to_html()
#' 
skim_data_to_html <- function(.df) {
  .df |>  
    skimr::skim() |> 
    skimr::partition()  |> 
    purrr::map(
      \(x) gt::gt(x) |>  gt_dark_mode()
    ) |> 
    htmltools::tagList() 
}