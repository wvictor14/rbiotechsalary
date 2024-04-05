#' new ui
#' @export
#' @examples \dontrun{
#'   launch_app(ui = rb_ui2, server = function(input, output) {})
#'   launch_app(ui = rb_ui2, server = rb_server_2)
#'   
#' }
rb_ui2 <- function() {
  link_github <- tags$a(
    shiny::icon("github"), "source",
    href = "https://github.com/wvictor14/rbiotechsalary",
    target = "_blank"
  )
  .colors <- list(
    'primary' = '#41AB5DFF'
  )
  page_navbar(
    theme = bs_theme(
      version = 5, 
      #presets = 'shiny', 
      'primary' = .colors$primary
    ),
    title = "r/biotech salary",
    fillable = FALSE,
    sidebar = sidebar(
      p('Choose your role'),
      filters_ui('filters')
    ),
    nav_panel(
      title = "Salaries", 
      
      layout_columns(
        value_box(
          title = "Average Salary", 
          value = uiOutput('text_average'),
          theme= 'text-success',
          #theme = value_box_theme(bg = "#287A28", fg = "#FFFFFF"),
          showcase = plotly::plotlyOutput('plot_sparkline_average'), 
          showcase_layout = "left center", 
          full_screen = TRUE,
          fill = TRUE, 
          height = NULL,
          uiOutput('text_ave_breakdown')
        ),
        value_box(
          title = "Number of Survey Respondents", 
          value = p('631'),
          theme= 'text-success',
          #theme = value_box_theme(bg = "#287A28", fg = "#FFFFFF"),
          showcase = bsicons::bs_icon('person-lines-fill'),
          showcase_layout = "left center", 
          full_screen = TRUE,
          fill = TRUE, 
          height = NULL
        ),
        value_box(
          title = "Placeholder", 
          value = p('Sixty-one'),
          theme= 'text-success',
          #theme = value_box_theme(bg = "#287A28", fg = "#FFFFFF"),
          showcase = bsicons::bs_icon('currency-exchange'),
          showcase_layout = "left center", 
          full_screen = TRUE,
          fill = TRUE, 
          height = NULL
        ),
      ),
      
      plotly::plotlyOutput("plot"),
      
      card(
        full_screen = TRUE,
        min_height = '400px',
        card_body(
          table_raw_ui("table_raw")
        )
      )
    ),
    nav_panel(
      title = "Career progression", 
      card(
        full_screen = TRUE,
        card_body(
          min_height = '300px',
          layout_columns(
            col_widths = c(4, 8),
            gt::gt_output('table_career_progression'),
            plotly::plotlyOutput('plot_experience')
          )
        )
      )
    ),
    nav_spacer(),
    nav_menu(
      title = "Links",
      align = "right",
      nav_item(link_github)
    ),
    nav_item(
      input_dark_mode(id = "dark_mode", mode = "dark")
    )
  )
  
}

#' updated server function
#' @export
rb_server_2 <- function(input, output, session) {
  
  .salaries <- filters_server('filters')
  table_salary_stats_server('table_salary_stats', .salaries)
  
  # value boxes
  output$plot_sparkline_average <- plotly::renderPlotly(
    .salaries() |>
      
      # take most recent 100 submissions for sparkline
      arrange(desc(date)) |> 
      slice(1:100) |> 
      plot_sparkline(color = '#41AB5DFF')
  )
  
  stats <- reactive({
    .salaries() |> 
      select(salary_total, salary_base, bonus) |> 
      summarize(across(everything(), ~mean(.x, na.rm = TRUE))) |>
      rename_with(
        ~stringr::str_remove(.x, 'salary_') |>  stringr::str_to_sentence(), 
        everything()
      ) |> 
      tidyr::pivot_longer(everything()) |> 
      mutate(
        .label = glue::glue("{name}: {round(value, digits = -3)/1000}K")
      )
  })
  output$text_average <- renderUI({
    .text <- stats() |> 
      filter(name == 'Total') |> pull(value) |>  round(digits = -3) |> 
      scales::dollar()
    HTML(paste0(.text, "<br>"))
  })
  
  output$text_ave_breakdown <- renderUI({
    HTML(
      stats() |> filter(name != 'Total') |> pull(.label) |>  paste0(collapse = '<br>') 
    )
  })
  
  output$plot <- plotly::renderPlotly({
    p <- plot_salary(.salaries()) + 
      labs(title = 'Total Compensation (Base + Bonus)')
    
    plotly::ggplotly(p) |> 
      plotly::config(displayModeBar = FALSE) |> 
      plotly::layout(margin = list(t = 0, b = 0, l = 0, r = 0))
  })
  
  
  output$table_career_progression <- gt::render_gt({
    gt_career_progression(.salaries())
  })
  output$plot_experience <- plotly::renderPlotly({
    
    # suppress warnings for app session
    storeWarn<- getOption("warn")
    options(warn = -1)
    
    if (nrow(.salaries()) > 0) {
      plot_experience(.salaries())
    } else {
      p <- ggplot() + theme_minimal()
      plotly::ggplotly(p)
    }
  })
  
  # render the table + other components
  table_raw_server('table_raw', .salaries, height = gt::px(400))
  
  observeEvent(input$dark_mode, {
    if (input$dark_mode == "dark") {
      showNotification("Welcome to the dark side!")
    }
  })
  
}