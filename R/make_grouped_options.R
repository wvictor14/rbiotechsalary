#' makes grouped options list for selectize input
#'
#' @export
#' @examples
#' make_grouped_options(salaries, title_category, title_general)
make_grouped_options <- function(.df, .groups, .choices) {
  .df |>
    distinct({{ .groups }}, {{ .choices }}) |>
    arrange({{ .groups }}, {{ .choices }}) |>
    summarize(.by = {{ .groups }}, choices = list(unique({{ .choices }}))) |>
    tibble::deframe()
}
