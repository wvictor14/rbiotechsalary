#' table of summarized salary info
#'
#' not currently used
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
    mutate(base_p = base / total) |>
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
    ) |>
    gtExtras::gt_plt_bar_pct(
      base_p,
      fill = pal_salary()['base'],
      background = pal_salary()['bonus']
    ) |>
    gt::cols_align(title_general, align = 'right') |>
    gt::cols_align(bonus_pct, align = 'left') |>

    # theming
    gt::tab_style(
      style = gt::cell_borders(style = 'hidden'),
      locations = gt::cells_body(
        columns = c(everything(), -title_category)
      )
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
      style = gt::cell_text(size = 'xx-small', color = "white", ),
      location = gt::cells_row_groups()
    ) |>

    gt::tab_options(
      column_labels.border.top.style = 'hidden',
      column_labels.border.bottom.style = 'hidden',
      row_group.padding = gt::px(0),
      data_row.padding = gt::px(1)
    )
}
