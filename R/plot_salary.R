
#' @rdname plot_salary
#' @export
calculate_salary_stats <- function(.df, x) {
  x_mean <- .df |>  pull({{x}}) |> mean()
  percentiles <- .df |>  pull({{x}}) |>  quantile(c(0.05, 0.95))
  stats <- c(
    'Average' = x_mean,
    stats::setNames(percentiles, nm = c('5th percentile', '95th percentile'))) |>
    tibble::enframe(name = 'stat', 'x') |>
    mutate(stat = forcats::fct(
      stat, levels = c('5th percentile', 'Average', '95th percentile')))
}

#' plot salary histogram
#' 
#' A ggplot2 version of the histogram. Not currently used in the app, 
#' see plot_salary_histogram for current implementation
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

#' generates binned data and stats for histogram plot
#' @export
#' @examples
#' salaries |> make_hist_data(salary_total)
#' salaries |> filter(title_general == 'Scientist') |> make_hist_data(salary_total)
#' 
make_hist_data <- function(.df, x) {
  # create plot data, start with hist to generate cuts and counts
  .plot_data <- .df |> pull({{x}}) |> 
    hist(plot = FALSE, breaks = seq(-0.5, 100, by = 1)*10000) |> 
    with(data.frame(
      stats::embed(breaks,2), 
      counts = counts,
      mids = mids
    )) |> 
    filter(counts > 0 ) |> 
    tibble::tibble() |> 
    mutate(bin = glue::glue(
      '${x}K - ${y}K',
      x = X2/1000, y = X1/1000
    )) |> 
    mutate(mids_lab = glue::glue("${mids/1000}K"))
  
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
  
  return(
    list(
      data = .plot_data,
      stats = .stats,
      stats_labs = .stats |> purrr::map(\(x) glue::glue("${x/1000}K"))
    )
  )
}
#' plot salary histogram v2
#' @export
#' @examples
#' 
#' # all
#' salaries |>  make_hist_data(salary_total) |>  plot_salary_histogram(font_color = 'black', hover_bg = 'white')
#' 
#' # scientist
#' salaries |>  filter(title_general == 'Scientist') |> make_hist_data(salary_total) |> plot_salary_histogram(font_color = 'black', hover_bg = 'white')
#'   
#' # 1
#' salaries |>  slice(1) |> make_hist_data(salary_total) |> 
#'   plot_salary_histogram(font_color = 'black', hover_bg = 'white')
plot_salary_histogram <- function(
    .plot_data, 
    color = '#41AB5DFF', font_color = '#EEE8D5', hover_bg = '#161C21',
    font_size = 16,
    ...) {
  
  # set up x axis, categorical, labels, coordinates ----
  
  # for categorical axis, need to get appropriate factor level
  # get the level of x from converted factor vector y
  .fct_levels <- .plot_data$data$mids_lab |>  
    as.character() |> forcats::fct_inorder() |> levels() 
  get_x_coord <- function(x, get_level_index = FALSE) {
    if (get_level_index) { 
      # minus 1 because plotly starts at 0
      out <- which(.fct_levels == as.character(x)) - 1 
      return(out)
    } else { return(as.character(x)) }
  }
  # make text annotations
  make_text <- function(text, x, y, color) {
    list(
      text = text, x = x, y = y, font = list(size = font_size), color = color, 
      yref = "paper", xanchor = "center", yanchor = "bottom", showarrow = FALSE
    )
  }
  
  annotations = list(
    make_text("Median", get_x_coord(.plot_data$stats_lab$med), 0.9, font_color),
    make_text("10th", get_x_coord(.plot_data$stats_lab$q10), 0.9, 'grey'),
    make_text("90th", get_x_coord(.plot_data$stats_lab$q90), 0.9, 'grey')
  )
  # make vline annotations
  vline <- function(x = 0, y1 = 0.9, color = "seagreen") {
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
  
  ## extend y axis range to zero and upper limit by multiplier
  .y_range <- c( 0, max(.plot_data$data$counts)*1.2) 
  
  ## get axis ticks and labels
  tick_values <- scales::extended_breaks(n=6)(1:length(.fct_levels))
  tick_labels <- .fct_levels[tick_values]
  
  ### remove NAs
  tick_values <- tick_values[!is.na(tick_labels)]
  tick_labels <- tick_labels[!is.na(tick_labels)]
  
  .plot_data$data |>
    mutate(mids_lab = factor(mids_lab)) |> 
    plotly::highlight_key(~mids_lab) |>  
    plotly::plot_ly(
      ...,
      x = ~mids_lab,
      y = ~counts,
      text = glue::glue("{.plot_data$data$bin}<br>{.plot_data$data$counts} jobs"),
      color = I(color),
      textposition = 'none',
      hoverinfo  = 'text',
      type = 'bar'
    )  |>
    plotly::highlight(
      on = "plotly_click", off = "plotly_doubleclick", opacityDim = 0.3,
      persistent = FALSE
      )  |> 
    plotly::config(displayModeBar = FALSE) |> 
    plotly::layout(
      barmode = 'overlay',
      margin = list(t = 25, b = 0, l = 0, r = 0),
      plot_bgcolor  = "rgba(0, 0, 0, 0)",
      paper_bgcolor = "rgba(0, 0, 0, 0)",
      yaxis = list(
        fixedrange = TRUE,
        visible = FALSE, 
        showgrid = FALSE,
        range = .y_range
      ),
      xaxis = list(
        fixedrange = TRUE,
        ticktext = as.list(tick_labels),
        tickvals = as.list(tick_labels),
        title = list(
          text = NULL
        )
      ),
      #dragmode = FALSE,
      font = list(color = font_color, size = font_size) ,
      hoverlabel = list(
        font = list(size=15, color = font_color), 
        bgcolor = hover_bg),
      bargap = 0.1,
      shapes = list(
        #vline(x = '130000', color = font_color), 
        vline(x = get_x_coord(.plot_data$stats_lab$med), color = font_color), 
        vline(x = get_x_coord(.plot_data$stats_lab$q10), color = 'grey'), 
        vline(x = get_x_coord(.plot_data$stats_lab$q90), color = 'grey')
      ),
      annotations = annotations
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
    .df, color = '#41AB5D', font_color = '#EEE8D5', bg_color = 'black',
    sizeref = 0.1) {
  
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
  
  plotly::plot_ly() |> 
    plotly::add_trace(
      x = .df$years_of_experience, y = .df$salary_total,
      type = 'scatter', mode = 'markers',
      hoverinfo = 'none',
      marker = list(
        color = 'darkgrey',
        opacity = 0.2
      ),
      showlegend = FALSE
    ) |> 
    plotly::add_trace(
      x = x, y = y, type = 'scatter', mode = 'markers', 
      hovertext = text, hoverinfo = 'text',
      marker = list(
        size = size,
        sizeref = sizeref,
        sizemin = 6,
        sizemode = 'area',
        color = color,
        opacity = 1,
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


