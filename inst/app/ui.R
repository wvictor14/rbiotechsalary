page_navbar(
  theme = bs_theme(
    version = 5, 
    fg = '#EEE8D5',
    bg =  '#232627',
    primary = .colors$primary,
    info = '#EEE8D5',
    "border-color" = 'rgba(255,255,255,0.1)', #495057',
    "border-color-translucent" = 'rgba(255,255,255,0.1)', #495057'
    "focus-ring-color" = "rgba(255,255,255,0.1)",
    "accordion-border-width" = "1px"
  ) |> 
    bs_add_rules(
      list("
          h1 {
            margin: -5px;
          }
             
          .selectize-input {
            border-width: 0px !important;
            background-color: #202020 !important;
            color: #EEE8D5 !important;
            overflow-y: auto;
            margin-top:-5px;
            margin-bottom:-10px;
            outline: none
          }
          ::-webkit-input-placeholder {
            color: #EEE8D5;
            padding-left: 5px;
          }
          .selectize-input.focus {
            border-color: white;
            border-width: 0px;
          }
          .selectize-dropdown-content {max-height: 90vh !important}
          "
      )
    ), 
  title = "r/biotech salaries",
  footer = tags$footer(
    style = "
        padding: 0px; 
        margin-bottom: 0px; 
        text-align: center; 
        position: fixed;
        bottom: 0;
        padding-bottom: 0px; 
        width: 100%;",
    p(
      style = "margin-top: 20px; color: #888;",
      glue::glue("rbiotechsalary {version}")
    )
  ),
  
  sidebar = sidebar(
    title = NULL,
    gap = '1px',
    width = 300,
    filters_ui('filters', .salaries = salaries)
  ),
  
  ### panel 1 ----
  nav_panel(
    title = "Salaries",
    htmltools::h1(shiny::textOutput('content_title_1')),
    layout_column_wrap(
      
      style = htmltools::css(grid_template_columns = "2fr 1fr"),
      plotly::plotlyOutput("plot_salary_histogram"),
      value_boxes_stats_ui('value_boxes')
    )
  ),
  
  ### table ----
  nav_panel(
    title = 'Table',
    htmltools::h1(shiny::textOutput('content_title_2')),
    table_raw_ui("table_raw")
  ),
  ### panel career progression ----
  nav_panel(
    title = "Career progression", 
    htmltools::h1('Average Total Compensation by Years of Experience'),
    card(
      full_screen = TRUE,
      card_body(
        min_height = '300px',
        plotly::plotlyOutput('plot_career_progression')
      )
    )
  ),
  
  ## panel info ----
  nav_panel(
    title = 'Info',
    htmltools::includeMarkdown(
      fs::path_package('rbiotechsalary', 'markdown', "info_page.md")
    ),
    tags$br(),
    tags$br()
  ),
  
  ## data pipe ----
  nav_panel(
    title = 'Raw Data',
    h2('Completeness and summary stats for original raw data'),
    reactable::reactableOutput('skim_raw_data')
  ),
  
  ## menu ----
  nav_spacer(),
  nav_menu(
    title = "Links",
    align = "right",
    nav_item(link_add_data),
    nav_item(link_google),
    nav_item(link_reddit),
    nav_item(link_github)
    
  ),
  nav_item(
    #input_dark_mode(id = "dark_mode", mode = "dark")
  )
)