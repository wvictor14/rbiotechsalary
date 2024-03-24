
#' @rdname plot_salary
#' @export
calculate_salary_stats <- function(.df, x) {
  x_mean <- .df |>  pull({{x}}) |> mean()
  percentiles <- .df |>  pull({{x}}) |>  quantile(c(0.05, 0.95))
  stats <- c(
    'Average' = x_mean,
    setNames(percentiles, nm = c('5th percentile', '95th percentile'))) |>
    tibble::enframe(name = 'stat', 'x') |>
    mutate(stat = forcats::fct(
      stat, levels = c('5th percentile', 'Average', '95th percentile')))
}

#' plot salary histogram
#'
#' @examples
#' library(r_biotech_salary)
#' data(salaries)
#' salaries_sub <- salaries |>
#'   dplyr::filter(title_general %in% c('Scientist', 'Principal Scientist'),
#'          stringr::str_detect(location_country, 'United States') )
#' plot_salary(salaries_sub)
#'
#' @param .df salary data
#' @export
plot_salary <- function(
    .df, x = salary_total, fill = title_general, title = '',
    .type = 'plotly') {

  .df <- .df |>  filter(!is.na({{x}}), !is.na({{fill}}))

  stats <- calculate_salary_stats(.df, {{x}})

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
    paletteer::scale_fill_paletteer_d(pal_paletteer(), guide ='none') +
    #004488FF #DDAA33FF #BB5566FF
    paletteer::scale_color_paletteer_d('khroma::highcontrast') +
    scale_y_continuous(
      label = ~scales::percent(.x, accuracy = 1),
      expand = expansion(mult = c(0, 0))
    ) +
    scale_x_continuous(label = scales::dollar, expand = expansion())  +
    labs(title = title, color = '', x = '', y = '% of jobs')

  stopifnot(.type %in% c('plotly', 'ggplot2'))
  if (.type == 'plotly') {
    p <- plotly::ggplotly(p, height = '250')
  }

  suppressWarnings({ p })
}


#' Create stats table for salary data
#'
#' @param .df salaries
#' @export
#' @examples
#' data(salaries)
#' salaries |> filter(title_general == 'Scientist') |>   gt_salary_stats()
#'
gt_salary_stats <- function(.df, .return_data = FALSE, .gt = TRUE) {

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


#' Plot salary against years of experience
#'
#' @param .df  salary data
#'
#' @return ggplot
#' @export
#'
#' @examples
#' data(salaries)
#' salaries_sci <- salaries |>
#'   filter(title_general == 'Scientist', location_country == 'United States Of America')
#' plot_experience(salaries_sci)
plot_experience <- function(.df, fill = title_general, .plotly =TRUE) {

  p <- .df |>
    arrange(years_of_experience) |>
    mutate(exp_binned = cut(
      years_of_experience,
      breaks = c(-Inf, 2, 5, 9, 14, Inf),
      labels = c('1-2', '3-5', '6-9', '10-14', '15+'))) |>
    ggplot(aes(x = exp_binned, y = salary_total, fill = {{fill}})) +
    geom_boxplot(outlier.shape = NA, position = position_dodge()) +
    theme_minimal() +
    theme(
      panel.border = element_blank(),
      axis.line = element_line(),
      axis.title.y = element_blank(),
      panel.grid.minor.y = element_line(),
      panel.grid.major.x = element_blank()) +
    paletteer::scale_fill_paletteer_d(pal_paletteer()) +
    scale_y_continuous(
      labels = scales::dollar, limits = c(0, NA),
      expand = expansion(c(0, 0.1))
    ) +
    labs(
      title = 'Total Compensation by Years of Experience',
      x = 'Years of Experience')

  if (.plotly) {
    p <- plotly::ggplotly(p, height = 250) |> plotly::layout(boxmode = "group")
  }

  suppressWarnings({ p })
}

#' plot career progression
#'
#' @examples
#' library(r_biotech_salary)
#' data(salaries)
#' salaries_sub <- salaries |>
#'   dplyr::filter(title_general %in% c('Scientist', 'Principal Scientist'),
#'          stringr::str_detect(location_country, 'United States') )
#' plot_salary(salaries_sub)
#'
#' @param .df salary data
#' @export
plot_career_progression <- function(
    .df, x = date, fill = title_general, title = '',
    .type = 'plotly') {
  .df %>%
    count({{x}}) |>
    plotly::plot_ly(x = ~{{x}}) |>
    plotly::add_lines(y = ~n) |>
    plotly::layout(
      xaxis = list(rangeslider = (list(type = 'date')))
    )
}


#' table of summarized salary info
#'
#' @return plot
#' @export
#'
#' @examples
#' gt_career_progression(salaries)
gt_career_progression <- function(.df) {
  .df |>
    arrange(title_general) |>
    group_by(title_category, title_general) |>
    dplyr::summarize(
      total = median(salary_total, na.rm = TRUE),
      base = median(salary_base, na.rm = TRUE),
      bonus = median(bonus, na.rm = TRUE),
      bonus_pct = mean(bonus_pct, na.rm = TRUE)
    ) |>
    #ungroup() |>
    mutate(base_p = base/total) |>
    relocate(base_p, .after = total) |>
    select(-base) |>
    gt::gt() |>

    # adjust content
    gt::cols_label(
      title_general = '',
      total = 'Total',
      base_p = 'Base/Bonus',
      bonus = 'Bonus'
    ) |>

    gt::fmt_currency(columns = c(total, bonus), decimals = 0) |>
    gt::fmt_percent(bonus_pct, decimals = 0) |>
    gt::cols_merge(
      columns = c(bonus, bonus_pct),
      pattern = "{1} ({2})"
      )|>
    gtExtras::gt_plt_bar_pct(
      base_p, fill = pal_salary()['base'], background = pal_salary()['bonus']
    ) |>
    gt::cols_align(title_general, align = 'right') |>
    gt::cols_align(bonus_pct, align = 'left') |>

    # theming
    gt::tab_style(
      style = cell_borders(style = 'hidden'),
      locations = cells_body(
        columns = c(everything(), -title_category))
    ) |>
    gt::tab_style(
      style = list(
        gt::cell_text(color = pal_salary()['total'])
      ),
      locations = gt::cells_body(
        columns = total
      )
    ) |>

    gt::tab_style(
      style = list(
        gt::cell_text(color = pal_salary()['bonus_2'])
      ),
      locations = gt::cells_body(
        columns = bonus
      )
    ) |>
    gt::tab_style(
      style = gt::cell_text(size = 'xx-small',
        color = "white",
      ),
      location = gt::cells_row_groups()
    ) |>

    gt::tab_options(
      column_labels.border.top.style = 'hidden',
      column_labels.border.bottom.style = 'hidden',
      row_group.padding = gt::px(0),
      data_row.padding = gt::px(1)
    )
}
