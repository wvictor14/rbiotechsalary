#' small table salary stats
#'
#' @param .df salaries
#' @export
#' @examples
#' data(salaries)
#' salaries |> filter(title_general == 'Scientist') |>   table_basic_salary_stats()
#'
table_basic_salary_stats <- function(.df) {
  .summarized <- .df |> 
    select(salary_total, salary_base, bonus) |> 
    summarize(
      across(everything(), ~mean(.x, na.rm = TRUE))
    ) |>
    
    rename_with(
      ~stringr::str_remove(.x, 'salary_') |>  stringr::str_to_sentence(), 
      everything()
    ) 
  .summarized |>
    mutate(across(everything(), ~round(.x, -3))) |> 
    gt::gt() |> 
    gt::fmt_currency(decimals = 0) |> 
    gt::tab_options(
      table.border.top.style = "hidden",
      table.border.bottom.style = "hidden",
      column_labels.border.bottom.style = 'none',
      row_group.border.top.style = 'none',
      table_body.border.top.style = 'none',
      heading.border.bottom.style = 'none',
      table_body.border.bottom.style = 'none'
    ) |> 
    gt::tab_style(
      style = gt::cell_borders(color = 'white', style =  'hidden'),
      locations = gt::cells_column_labels()
    )  |> 
    gt::tab_style(
      style = list(
        gt::cell_fill('#A1D99BFF')
      ),
      locations = list(
        gt::cells_column_labels(columns = Total),
        gt::cells_body(columns = Total)
      )
    ) |> 
    
    gt::tab_style(
      style = list(
        gt::cell_fill('#C7E9C0FF')
      ),
      locations = list(
        gt::cells_column_labels(columns = Base),
        gt::cells_body(columns = Base)
      )
    ) |> 
    
    gt::tab_style(
      style = list(
        gt::cell_fill('#E5F5E0FF')
      ),
      locations = list(
        gt::cells_column_labels(columns = Bonus),
        gt::cells_body(columns = Bonus)
      )
    )
}

#' small plot salary stats
#'
#' @param .df salaries
#' @export
#' @examples
#' data(salaries)
#' salaries |> filter(title_general == 'Scientist') |>   table_basic_salary_stats()
#'
plot_basic_salary_stats <- function(.df) {
  
  .summarized <- .df |> 
    select(salary_total, salary_base, bonus) |> 
    summarize(
      across(everything(), ~mean(.x, na.rm = TRUE))
    ) |>
    
    rename_with(
      ~stringr::str_remove(.x, 'salary_') |>  stringr::str_to_sentence(), 
      everything()
    ) |> 
    tidyr::pivot_longer(
      everything()
    )
  p <- .summarized |>
    filter(name != 'Total') |> 
    mutate(title = 'all',
           name = forcats::fct(name, c('Total', 'Base', 'Bonus')) |> 
             forcats::fct_rev(),
           .label = stringr::str_c('$',round(value, -3)/1000, 'K'))  |> 
    ggplot(aes(x = value, y = title, fill = name)) +
    geom_col() +
    ggrepel::geom_text_repel(aes(label = .label), position = position_stack()) +
    theme_void()  +
    scale_fill_discrete(guide = 'none')
  p |>  plotly::ggplotly()
  
  
  .summarized |> 
    filter(name != 'Total') |> 
    mutate(
      title = 'all',
      name = forcats::fct(name, c('Total', 'Base', 'Bonus')),
      .label = glue::glue('{name}\n${round(value, -3)/1000}K')
    )  |> 
    plotly::plot_ly() %>%
    plotly::add_bars(
      x = ~value, y = ~title, color = ~name,
      text = ~.label, 
      hoverinfo = 'text', 
      showlegend = FALSE,
      colors = colors_green_3,
      marker = list(
        cornerradius = '100%' # not available until R updates to latest plotly.js 2.29
      )
    )  |> 
    plotly::layout(
      barmode = 'stack',
      xaxis = list(visible = F, showgrid = F, title = ""),
      yaxis = list(visible = F, showgrid = F, title = ""),
      hovermode = 'unified_x',
      margin = list(t = 0, r = 0, l = 0, b = 0),
      font = list(color = "white"),
      paper_bgcolor = "transparent",
      plot_bgcolor = "transparent"
    ) |> 
    plotly::config(displayModeBar = F) 
  
  p <- .summarized |> 
    filter(name != 'Total') |> 
    mutate(
      title = 'all',
      name = forcats::fct(name, c('Total', 'Base', 'Bonus')),
      .label = glue::glue('{name}\n${round(value, -3)/1000}K')
    ) |> 
    ggplot(aes(x = .label, y = title)) +
    geom_tile() +
    geom_point(aes(color = value, size = value))+
    theme_void() +
    scale_color_gradient(high = '#00441BFF', low = colors_green_3[1], guide = 'none') +
    scale_size_continuous(range = c(5,15), guide = 'none') +
    scale_x_discrete(position = 'top') +
    theme(axis.text.x =element_text(), aspect.ratio = 1)
  plotly::ggplotly(p)
}

#' sparkline for salary average showcase value box 
#' 
#' @export
plot_sparkline <- function(.df, color = 'white') {
  
  .df |> 
    plotly::plot_ly() %>%
    plotly::add_lines(
      x = ~date, y = ~salary_total,
      color = I(color), span = I(1),
      fill = 'tozeroy', alpha = 0.2
    ) %>%
    plotly::layout(
      xaxis = list(visible = F, showgrid = F, title = ""),
      yaxis = list(visible = F, showgrid = F, title = ""),
      hovermode = "x",
      margin = list(t = 0, r = 0, l = 0, b = 0),
      font = list(color = color),
      paper_bgcolor = "transparent",
      plot_bgcolor = "transparent"
    ) %>%
    plotly::config(displayModeBar = F) %>%
    htmlwidgets::onRender(
      "function(el) {
      var ro = new ResizeObserver(function() {
         var visible = el.offsetHeight > 200;
         Plotly.relayout(el, {'xaxis.visible': visible});
      });
      ro.observe(el);
    }"
    )
}