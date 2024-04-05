#' stylize to dark mode
#'
#' @param gt_object gt table
#' @param ... tab options
#'
#' @return gt table
#' @export
#'
#' @examples
#'   
#' salaries |> slice(1:20) |>  gt_table_raw() |>  gt_dark_mode()
#' salaries |> slice(1:20) |>  gt_table_raw() |>  
#' gt::tab_style(
#' style = gt::cell_fill(alpha = 0), locations = list(gt::cells_body()))  |> 
#' gt::opt_interactive()
#' 
gt_dark_mode <- function(
    gt_object, .color_dark = '#00000000', .color_light = '#EEE8D5', ...) {
  stopifnot("'gt_object' must be a 'gt_tbl', have you accidentally passed raw data?" = "gt_tbl" %in% class(gt_object))
  
  gt_object %>%
    gt::tab_options(
      heading.border.bottom.style = "none",
      table.background.color = .color_dark,
      table.font.color = .color_light,
      table.border.top.style = "none",
      table_body.border.top.style = "none",
      table_body.border.bottom.color = .color_dark,
      column_labels.border.top.style = "none",
      column_labels.background.color = .color_dark,
      column_labels.border.bottom.color = .color_light,
      data_row.padding = gt::px(1),
      ...
    )
}