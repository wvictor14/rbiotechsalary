#' sparkline for salary average showcase value box
#'
#' @export
#' @examples
#'
#'
#' plot_sparkline(salaries, x = date, y = salary_total, color = 'seagreen')
#'
#' # user density
#' salaries |>
#'   count(date) |>
#'   mutate(x = cumsum(n))  |>
#'   plot_sparkline(date, x, color = 'seagreen')
#'
#'
plot_sparkline <- function(.df, x, y, color = 'white') {
  .x <- .df |> pull({{ x }})
  .y <- .df |> pull({{ y }})

  plotly::plot_ly() %>%
    plotly::add_lines(
      x = .x,
      y = .y,
      color = I(color),
      span = I(1),
      fill = 'tozeroy',
      alpha = 0.2
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
