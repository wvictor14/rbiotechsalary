#' plot salary histogram
#'
#' @examples
#' library(r_biotech_salary)
#' data(salaries)
#' plot_salary(salaries)
#'
#' @param .df salary data
#' @export
plot_salary <- function(.df, x = salary_total, fill = title_general, title = FALSE) {

  x_mean <- .df |>  pull({{x}}) |> mean()
  x_mean_lab <- x_mean |>  gt::vec_fmt_currency(decimals = 0)

  p <- ggplot(.df, aes(
    x = {{x}}, fill = {{fill}})) +
    geom_vline(xintercept = x_mean, color = 'darkblue', linetype = 'twodash') +
    annotate(
      geom = 'text', label =  glue::glue("Average is {x_mean_lab}"),
      color = 'darkblue',

      # relative coordinates
      x = x_mean+(x_mean*0.1), y = Inf
    ) +
    geom_histogram(bins = 25, aes(y = after_stat(count / sum(count))))  +
    scale_fill_discrete(guide ='none') +
    theme_minimal() +
    theme(panel.grid.minor = element_blank(),
          axis.line = element_line()) +
    scale_y_continuous(
      label = ~scales::percent(.x, accuracy = 1),
      expand = expansion(mult = c(0, 0))
    ) +
    scale_x_continuous(label = scales::dollar, expand = expansion())  +
    labs(
      title = glue::glue("Average is {x_mean_lab}/year"),
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
plot_salary_title <- function(.df, .return_data = FALSE) {

  l <- list(
    average = mean(.df$salary_total),

    base_min = min(.df$salary_base),
    total_min = min(.df$salary_total),
    bonus_min = min(.df$bonus),

    base_max = max(.df$salary_base),
    total_max = max(.df$salary_total),
    bonus_max = max(.df$bonus)
  ) |>
    purrr::map(~scales::dollar(.x, accuracy = 1))

  if (.return_data) { return(l) }


  title <- tibble(
    left = c('Average is ', 'Total Pay Range ', 'Base Pay ', 'Bonus '),
    right = c(
      "{l$average} /yr", "{l$total_min} - {l$total_max} /yr",
      "{l$base_min} - {l$base_max} /yr", "{l$bonus_min} - {l$bonus_max} /yr")
  ) |>
    mutate(
      left = stringr::str_pad(
          left, max(nchar(left)) + 4, side = 'right'),
      right = purrr::map_chr(right, ~glue::glue(.x)) %>%
        stringr::str_pad(max(nchar(.)), 'left')) |>
    rowwise() |>
    mutate(string = glue::glue(left, right)) |>
    pull(string) |>
    paste0(collapse = '\n')
  return(title)
}
