#' plot salary histogram
#'
#' @examples
#' library(r_biotech_salary)
#' data(salaries)
#' plot_salary(salaries)
#'
#' @param .df salary data
#' @export
plot_salary <- function(.df, x = salary_total, fill = title_general, title = NULL) {
  .df <- .df |>  filter(!is.na({{x}}), !is.na({{fill}})
  )
  x_mean <- .df |>  pull({{x}}) |> mean()
  x_mean_lab <- x_mean |>  gt::vec_fmt_currency(decimals = 0)
  if (is.null(title)) {
    title <- glue::glue("Average is {x_mean_lab}/year")
  } else {
    title <- title
  }

  # stats
  percentiles <- .df |>  pull({{x}}) |>  quantile(c(0.05, 0.95))
  stats <- c(
    'Average' = x_mean,
    setNames(percentiles, nm = c('5th percentile', '95th percentile'))) |>
    tibble::enframe(name = 'stat', 'x') |>
    mutate(stat = forcats::fct(
      stat, levels = c('5th percentile', 'Average', '95th percentile')))

  p <- ggplot(.df, aes(
    x = {{x}}, fill = {{fill}})) +
    geom_vline(
      data = stats, aes(xintercept = x, color = stat), linetype = 'twodash') +
    geom_histogram(bins = 25, aes(y = after_stat(count / sum(count))))  +
    theme_minimal() +
    theme(
      axis.title.y = element_text(angle = 0, vjust = 0.5),
      panel.grid.minor = element_blank(),
      axis.line = element_line(),
      legend.position = 'bottom') +
    paletteer::scale_fill_paletteer_d('ggthemes::Tableau_20', guide ='none') +
    #004488FF #DDAA33FF #BB5566FF
    paletteer::scale_color_paletteer_d('khroma::highcontrast') +
    scale_y_continuous(
      label = ~scales::percent(.x, accuracy = 1),
      expand = expansion(mult = c(0, 0))
    ) +
    scale_x_continuous(label = scales::dollar, expand = expansion())  +
    labs(
      title = title, color = '',
      x = '', y = '% of jobs')
  plotly::ggplotly(p, height = 300)
}


#' Make the title for salary histogram
#'
#' @param .df salaries
#' @export
#' @examples
#' data(salaries)
#' salaries |>  plot_salary_title()
#'
plot_salary_title <- function(.df, .return_data = FALSE, .gt = TRUE) {

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
    left = c('Average is', 'Total Pay Range', 'Base Pay', 'Bonus'),
    right = c(
      '<b><a style="color:#DDAA33FF">{l$average}</b></a> /yr',
      glue::glue(
        '<b><a style="color:#004488FF">{l$total_min}</b></a>',
        ' - ',
        '<b><a style="color:#BB5566FF">{l$total_max}</b></a> /yr'
      ),
      "{l$base_min} - {l$base_max} /yr", "{l$bonus_min} - {l$bonus_max} /yr")
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


#' Plot salary against years of experience
#'
#' @param .df  salary data
#'
#' @return ggplot
#' @export
#'
#' @examples
#' data(salaries)
#' salaries_sci <- salaries |> filter(title_general == 'Scientist')
#' plot_salary_and_experience(salaries_sci)
plot_salary_and_experience <- function(.df) {

  .df |>  count(years_of_experience)
  .df |> glimpse()

}
