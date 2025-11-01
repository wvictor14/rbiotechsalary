bslib::page_navbar(
  theme = bs_theme(
    version = 5,
    font_scale = 0.7, # reduce by 20%
    fg = '#EEE8D5',
    bg = '#232627',
    primary = .colors$primary,
    info = '#EEE8D5',
    #"border-color" = 'rgba(255,255,255,0.1)', #495057',
    "border-color-translucent" = 'rgba(255,255,255,0.1)', #495057'
    "focus-ring-color" = "rgba(255,255,255,0.1)",
    "accordion-border-width" = "1px",
  ) |>

    bs_add_variables(
      "bslib-value-box-horizontal-break-point" = "1px" #freezes value boxes layout
    ) |>
    bs_add_rules(
      list(
        "
          h1, h2, h3 {
            
          }
             
          .selectize-input {
            border-width: 0px !important;
            background-color: #202020 !important;
            color: #EEE8D5 !important;
            overflow-y: auto;
            outline: none
          }
          .form-group {
          }
          .accordion-body {
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
          .bslib-value-box .value-box-value {
            font-size: 1rem
          }
          "
      )
    ),
  padding = c(0, 0),
  fillable = FALSE,
  title = "r/biotech salaries",
  sidebar = sidebar(
    title = 'Filter Salaries Dataset',
    open = 'desktop',
    gap = '1px',
    width = 300,
    filters_ui('filters', .salaries = salaries)
  ),
  header = htmltools::tags$script(
    "data-goatcounter" = "https://victor2wy.goatcounter.com/count",
    "async src" = "//gc.zgo.at/count.js"
  ),
  ### panel 1 ----
  nav_panel(
    title = "Salaries",
    fillable = FALSE,
    padding = 0,
    class = 'p-0 m-0 row-gap-2',
    #gap = '0px',
    htmltools::h3(shiny::textOutput('content_title_1')),
    card(
      class = 'p-0',
      full_screen = FALSE,
      gap = 0,
      #height = '400px',
      card_body(
        class = 'p-0',
        fill = FALSE,
        layout_column_wrap(
          width = 1 / 3,
          !!!value_boxes_stats_ui(
            'value_boxes',
            height = '130px',
            fg = '#EEE8D5'
          )
        )
      )
    ),
    card(
      full_screen = FALSE,
      max_height = '200px',
      card_body(
        class = 'p-0',
        #fill = FALSE,
        plotly::plotlyOutput("plot_salary_histogram")
      )
    ),

    card(
      card_title(
        class = 'd-flex justify-content-center',
        'Recently Submitted Salaries'
      ),
      card_body(
        class = 'p-0 m-0',
        table_raw_ui(class = 'p-0 m-0', "table_raw")
      )
    ),

    ### career progression ----
    br(),
    br(),
    htmltools::h3('Average Total Compensation by Years of Experience'),
    card(
      full_screen = TRUE,
      card_body(
        class = 'm-0 p-0',
        min_height = '300px',
        plotly::plotlyOutput('plot_career_progression')
      )
    ),

    ### top companies ----
    br(),
    br(),
    layout_columns(
      card(
        class = 'border border-secondary',
        htmltools::h3('Top Paying Companies'),
        card_body(
          reactable::reactableOutput('top_companies_tbl')
        )
      ),
      card(
        class = 'border border-secondary p-0',
        htmltools::h3('Top Paying Locations'),
        card_body(
          reactable::reactableOutput('top_locations_tbl')
        )
      )
    )
  ),
  
  nav_panel(
    title = "Companies",
    companies_ui(
      'companies',
      choices = select_companies_choices
    )
  ),

  ## panel info ----
  #nav_panel(
  #  title = 'Info',
  #  htmltools::includeMarkdown(
  #    fs::path_package('rbiotechsalary', 'markdown', "info_page.md")
  #  ),
  #   tags$br(),
  #   tags$br()
  # ),
  #
  # ## data pipe ----
  # nav_panel(
  #   title = 'Raw Data',
  #   h3('Completeness and summary stats for original raw data'),
  #   reactable::reactableOutput('skim_raw_data')
  # ),

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
