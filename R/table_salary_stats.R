table_salary_stats_ui <- function(id, ...) {
  tagList(
    gt::gt_output(NS(id, 'salary_stats'), ...)
  )
}
table_salary_stats_server <- function(id, .salaries) {
  stopifnot(is.reactive(.salaries))
  moduleServer(id, function(input, output, session) {
    
    output$salary_stats <- gt::render_gt({
      if (nrow(.salaries()) > 0) {
        table_salary_stats(.salaries())
      } else {
        tibble('No matching salary data' = '') |> 
          gt::gt() |>
          gt::tab_options(
            table.border.top.style = "hidden",
            table.border.bottom.style = "hidden")
      }
    })
  })
}


#' Create stats table for salary data
#'
#' @param .df salaries
#' @export
#' @examples
#' data(salaries)
#' salaries |> filter(title_general == 'Scientist') |>   table_salary_stats()
#'
table_salary_stats <- function(.df, .return_data = FALSE, .gt = TRUE) {
  
  l <- list(
    average = mean(.df$salary_total),
    
    base_min = quantile(.df$salary_base, 0.05),
    total_min = quantile(.df$salary_total, 0.05),
    bonus_min = quantile(.df$bonus, 0.05),
    
    base_max = quantile(.df$salary_base, 0.95),
    total_max = quantile(.df$salary_total, 0.95),
    bonus_max = quantile(.df$bonus, 0.95)
  ) |>
    purrr::map(~scales::dollar(.x, accuracy = 1))
  
  if (.return_data) { return(l) }
  
  
  title <- tibble(
    left = c('Average Total Pay', 'Total Pay Range', 'Base Pay', 'Bonus'),
    right = c(
      '<b><a style="color:#DDAA33FF">{l$average}</b></a>',
      glue::glue(
        '<b><a style="color:#004488FF">{l$total_min}</b></a>',
        ' - ',
        '<b><a style="color:#BB5566FF">{l$total_max}</b></a>'
      ),
      "{l$base_min} - {l$base_max}", "{l$bonus_min} - {l$bonus_max}")
  ) |>
    mutate(right = purrr::map_chr(right, ~glue::glue(.x)))
  
  if (.gt) {
    title <- title |>
      gt::gt() |>
      gt::cols_align(align = 'right', columns = right) |>
      gt::tab_options(
        data_row.padding = gt::px(0),
        table.border.top.style = "hidden",
        table.border.bottom.style = "hidden",
        column_labels.hidden = TRUE) |>
      gt::tab_style(
        style = gt::cell_borders(
          sides = c("top", "bottom"),
          color = "white",
          weight = gt::px(1.5),
          style = "solid"
        ),
        locations = gt::cells_body()
      ) |>
      gt::tab_header(title = gt::md("Yearly salary")) |>
      gt::fmt_markdown()
  }
  
  return(title)
}
