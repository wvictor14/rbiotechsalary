
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
#' library(rbiotechsalary)
#' data(salaries)
#' salaries_sub <- salaries |>
#'   dplyr::filter(title_general %in% c('Scientist', 'Principal Scientist'),
#'          stringr::str_detect(location_country, 'United States') )
#' plot_salary(salaries_sub)
#'
#' @param .df salary data
#' @export
plot_salary <- function(
    .df, x = salary_total, fill = title_general) {
  
  .df <- .df |>  filter(!is.na({{x}}), !is.na({{fill}}))
  stats <- calculate_salary_stats(.df, {{x}})
  
  p <- ggplot(.df, aes(
    x = {{x}}, fill = {{fill}})) +
    geom_vline(
      xintercept = stats$x[1],
      linetype = 'twodash',
      color = '#004488FF'
    ) +
    geom_vline(
      xintercept = stats$x[2],
      linetype = 'twodash',
      color = '#DDAA33FF'
    ) +
    geom_vline(
      xintercept = stats$x[3],
      linetype = 'twodash',
      color = '#BB5566FF'
    ) +
    geom_histogram(bins = 25, aes(y = after_stat(count / sum(count))))  +
    theme_minimal() +
    theme(
      axis.title.y = element_text(angle = 0, vjust = 0.5),
      panel.grid.minor = element_blank(),
      axis.line = element_line(),
      legend.position = 'bottom') +
    paletteer::scale_fill_paletteer_d(pal_paletteer(), guide ='none') +
    scale_y_continuous(
      label = ~scales::percent(.x, accuracy = 1),
      expand = expansion(mult = c(0, 0))
    ) +
    scale_x_continuous(label = scales::dollar, expand = expansion())  +
    labs(color = '', x = '', y = '% of jobs')
  
  p
}

#' plot salary histogram v2
#' @export
#' @examples
#' plot_salary_histogram(salaries, x = salary_total, font_color = 'black', hover_bg = 'white')
plot_salary_histogram <- function(
    .df, x, color = '#41AB5DFF', font_color = '#EEE8D5', hover_bg = '#161C21') {
  
  # create plot data
  .plot_data <- .df |> 
    pull(salary_total) |>  
    hist(plot = FALSE, breaks = 30) |>  
    as.list() |> 
    purrr::keep_at(c('counts', 'mids')) |>  
    bind_cols()  
  
  ## stats
  .stats <- .df |> 
    summarize(med = median({{x}}, na.rm = TRUE),
              q10 = quantile({{x}}, c(0.1)),
              q90 = quantile({{x}}, c(0.9))) |> 
    tidyr::pivot_longer(everything()) |>  tibble::deframe() |>  as.list() 
  
  # round to nearest bin
  .stats <- .stats |>  
    purrr::map(\(x) {
      ind <- abs(.plot_data$mids - x) |>  which.min()
      .plot_data |> slice(ind) |>  dplyr::pull(mids)
    })
  
  annotations = list(
    list(
      text = "Median", x = .stats$med, y = 0.8, yref = "paper",
      color = font_color, font = list(size = 20),
      xanchor = "center", yanchor = "bottom", showarrow = FALSE
    ),
    list(
      text = "10th", x = .stats$q10, y = 0.8, yref = "paper",
      font = list(size = 20, color = 'grey'),
      xanchor = "center", yanchor = "bottom", showarrow = FALSE
    ),
    list(
      text = "90th", x = .stats$q90, y = 0.8, yref = "paper",
      color = 'grey', font = list(size = 20),
      xanchor = "center", yanchor = "bottom", showarrow = FALSE
    )
  )
  
  ## extend y axis to zero and upper limit by 10%
  .y_range <- c( 0, max(.plot_data$counts)*1.45 ) |> round()
  
  plotly::plot_ly() |> 
    
    plotly::add_bars(
      x = .plot_data$mids,
      y = .plot_data$counts,
      color = I(color),
      hovertemplate ='Salary Range: %{x}<br>%{y} jobs<extra></extra>'
    ) |> 
    plotly::config(displayModeBar = FALSE) |> 
    plotly::layout(
      margin = list(t = 0, b = 0, l = 0, r = 0),
      plot_bgcolor  = "rgba(0, 0, 0, 0)",
      paper_bgcolor = "rgba(0, 0, 0, 0)",
      yaxis = list(
        fixedrange = TRUE,
        visible = FALSE, 
        showgrid = FALSE,
        range = .y_range
      ),
      xaxis = list(title = ''),
      font = list(color = font_color, size = 20) ,
      hoverlabel = list(
        font = list(size=15, color = font_color), 
        bgcolor = hover_bg),
      bargap = 0.1,
      dragmode = FALSE,
      shapes = list(
        vline(.stats$med, color = font_color), 
        vline(.stats$q10, color = 'grey'), 
        vline(.stats$q90, color = 'grey')
      ),
      annotations = annotations
    )
}

vline <- function(x = 0, y1 = 0.8, color = "seagreen") {
  list(
    type = "line",
    y0 = 0,
    y1 = y1,
    yref = "paper",
    x0 = x,
    x1 = x,
    width = -2,
    line = list(color = color, dash = "dash")
  )
}

#' plot career progression
#'
#' @examples
#' library(rbiotechsalary)
#' data(salaries)
#' salaries_sub <- salaries |>
#'   dplyr::filter(title_general %in% c('Scientist', 'Principal Scientist'),
#'          stringr::str_detect(location_country, 'United States') )
#' plot_career_progression(salaries_sub)
#'
#' @param .df salary data
#' @export
plot_career_progression <- function(
    .df, x = years_of_experience, y = salary_total,
    color = '#41AB5D', font_color = '#EEE8D5', bg_color = 'black') {
  
  summarized <- .df |> 
    summarize(
      .by = years_of_experience,
      salary_total = mean(salary_total),
      n = n()
    ) |> 
    filter(if_all(everything(), ~!is.na(.x))) |> 
    mutate(.text = glue::glue(
      '${sal}', '{years_of_experience} years of experience',
      '{n} salaries',
      .sep = '<br>',
      sal = round(salary_total, -3) |>  scales::number(big.mark = ',')))
  
  x <- summarized$years_of_experience;
  y <- summarized$salary_total;
  size <- summarized$n
  text <- summarized$.text
  
  plotly::plot_ly(
    x = x, y = y, type = 'scatter', mode = 'markers', 
    hovertext = text, hoverinfo = 'text',
    marker = list(
      size = size,
      sizeref = 0.05,
      sizemin = 6,
      sizemode = 'area',
      color = color,
      opacity = 0.90,
      cliponaxis = FALSE,
      line = list(width = 0)
    )
  ) |> 
    plotly::config(displayModeBar = FALSE) |> 
    plotly::layout(
      margin = list(t = 0, b = 0, l = 0, r = 0),
      plot_bgcolor  = bg_color, #"rgba(0, 0, 0, 0)",
      paper_bgcolor = bg_color, #"rgba(0, 0, 0, 0)",
      yaxis = list(
        visible = TRUE, 
        showgrid = FALSE, 
        layer = 'below traces',
        tickprefix = '$',
        rangemode = "tozero",
        showspikes = TRUE,
        spikecolor = '#EEE8D5',
        spikethickness = -2,
        spikedistance = 1,
        spikesides = FALSE
      ),
      xaxis = list(
        title = 'Years of Experience',  
        showgrid = FALSE, 
        zeroline = FALSE, 
        layer = 'below traces',
        showspikes = TRUE,
        spikecolor = '#EEE8D5',
        spikedistance = 1,
        spikethickness = -2
      ),
      font = list(color = font_color, size = 20),
      hovermode = 'x',
      hoverlabel = list(
        font = list(size=15, color = font_color), 
        bgcolor = '#161C21'
      )
    )
}


