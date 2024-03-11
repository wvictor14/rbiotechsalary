
#' @examples
#' library(r_biotech_salary)
#' data(salaries)
#' plot_salary(salaries)
#'
#' @param .df salary data
#' @export
plot_salary <- function(.df, x = salary_base, fill = title_general) {

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
    labs(x = glue::glue("Average is {x_mean_lab}/year"), y = '% of jobs') +
    scale_y_continuous(
      label = ~scales::percent(.x, accuracy = 1),
      expand = expansion(mult = c(0, 0))
      ) +
    scale_x_continuous(label = scales::dollar, expand = expansion())
  plotly::ggplotly(p, height = 300)
}
