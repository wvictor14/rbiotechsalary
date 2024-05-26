page_navbar(
  theme = bs_theme(
    version = 5, 
    font_scale = 0.8, # reduce by 20%
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
          h1, h2, h3 {
            margin: -5px;
          }
             
          .selectize-input {
            border-width: 0px !important;
            background-color: #202020 !important;
            color: #EEE8D5 !important;
            overflow-y: auto;
            padding-top: 0;
            padding-bottom: 0;
            outline: none
          }
          .form-group {
            margin-bottom: 0;
          }
          .accordion-body {
            padding-top: 0; padding-bottom: 0;
          }
          ::-webkit-input-placeholder {
            color: #EEE8D5;
            padding-left: 5px;
          }
          ::-webkit-scrollbar {
            width: 4px;
            height: 4px;
            border: 1px solid darkgrey;
          }
          ::-webkit-scrollbar-track {
            border-radius: 0;
            background: #202020;
          }
          
          ::-webkit-scrollbar-thumb {
            border-radius: 0;
            background: darkgrey;
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
  sidebar = sidebar(
    title = NULL,
    gap = '1px',
    width = 300,
    filters_ui('filters', .salaries = salaries)
  ),
  
  ### panel 1 ----
  nav_panel(
    title = "Salaries", 
    page_fillable(
      class = 'row-gap-0',
      #gap = '0px',
      htmltools::h2(shiny::textOutput('content_title_1')),
      card(
        class = 'p-0 mb-0 row-gap-0',
        full_screen = FALSE,
        gap = 0,
        #height = '400px',
        card_body(
          class = 'p-0',
          fill = FALSE,
          layout_column_wrap(
            width = 1/3, 
            !!!value_boxes_stats_ui('value_boxes', height = '100px')
          )
        ),
        card_body(
          class = 'p-0',
          #fill = FALSE,
          plotly::plotlyOutput("plot_salary_histogram", height = 'auto')
        )
      ),
      card(
        full_screen = TRUE,
        height = '250px', 
        class = 'p-0 mt-0',
        card_body(
          class = 'p-0',
          table_raw_ui("table_raw")
        )
      )
    )
  ),
  ### panel career progression ----
  nav_panel(
    title = "Career progression", 
    htmltools::h3('Average Total Compensation by Years of Experience'),
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
    h3('Completeness and summary stats for original raw data'),
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
        style = "color: #888; padding: 0px; font-size:1rem",
        glue::glue("rbiotechsalary {version}")
  )
)
