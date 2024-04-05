
#' Make grouped choices for shiny.fluent::dropDown
#'
#' @export
#' @examples
#' make_grouped_options(salaries, title_category, title_general)
#' @param .df salaries
#' @param .groups group column
#' @param .choices options column
make_grouped_options <- function(.df, .groups, .choices) {

  .df_choices <- .df |>
    mutate(key = {{.choices}}, text = as.character(key)) |>
    select({{.groups}}, key, text) |>
    distinct()  |>
    arrange(key) |>
    mutate(key = as.character(key)) |>
    mutate(itemType = list(NULL))

  .df_headers <- .df_choices |>
    group_by({{.groups}}) |>
    summarize(
      itemType = list(DropdownMenuItemType("Header"))
    ) |>
    mutate(key = stringr::str_c( 'Header', {{.groups}}), text = {{.groups}})

  bind_rows(.df_headers, .df_choices) |>
    arrange({{.groups}}, itemType)
}

DropdownMenuItemType <- function(type) {
  shiny.fluent::JS(paste0("jsmodule['@fluentui/react'].DropdownMenuItemType."), type)
}

#' makes grouped options list for selectize input
#' 
#' @export
#' @examples
#' make_grouped_options_2(salaries, title_category, title_general)
make_grouped_options_2 <- function(.df, .groups, .choices) {
  salaries |> 
    distinct({{.groups}}, {{.choices}}) |> 
    summarize(.by = {{.groups}}, choices = list(unique({{.choices}}))) |> 
    tibble::deframe()
}
