#' show hide button on reactable
#' 
#' taken from https://github.com/glin/reactable/issues/319
showHideButton = function(id, .table_id = 'table-raw' ){
  
  actionButton(
    id,
    "Show/hide additional columns",
    onclick = glue::glue(
      .open = '{{', .close = '}}',
      "Reactable.setHiddenColumns('{{.table_id}}', prevColumns => {
        return prevColumns.length === 0 ? ['Job title', 'Job details', 'Field', 'Stock', 'Highest education'] : []
      })"
    ),
    style = '
      color: grey;
      display: flex;
      float: left; 
      width: fit-content; height:40px; 
      margin-top: 0px;  margin-bottom: 0px;
      align: left; 
      padding-top:0.5rem; padding-bottom:0.5rem; 
      padding-left: 14px; padding-right: 16px;
      border:none;
    ',
    class = 'btn btn-dark'
  )
}

#' table_raw ui
#' @export
table_raw_ui <- function(id, ...) {
  tagList(
    #gt::gt_output(NS(id, "table_raw", ...))
    div(
      style = 'display: flex; flex-flow: column wrap;',
      reactable::reactableOutput(NS(id, "table_raw"), ...)
      #showHideButton(NS(id, 'toggle_button'))
    )
  )
}

#' table_raw server 
#' @export
table_raw_server <- function(id, .salaries,  defaultPageSize = 10, ...) {
  stopifnot(is.reactive(.salaries))
  moduleServer(id, function(input, output, session) {
    
    output$table_raw <- reactable::renderReactable(#gt::render_gt(
      ...,
      expr  = {
        if (nrow(.salaries()) == 0) return(NULL)
        
        .salaries() |>
          arrange(desc(date)) |>
          #slice(.slice) |>
          rt_table_raw(
            server = TRUE, searchable = TRUE, 
            defaultPageSize = defaultPageSize)
        #  gt_table_raw() |>  
        #  gt_dark_mode() |> 
        #  gt::opt_interactive()
      }
    )
    
    
    observeEvent(input$toggle_button, {
      # Send a message to the JavaScript handler when the button is clicked
      session$sendCustomMessage('toggleColumns', NULL)
    })
  })
}

#' gt table showing raw data
#' 
#' @export
#' @examples
#' 
#' salaries |> slice(1:20) |>  gt_table_raw()
#' 
gt_table_raw <- function(.df) {
  .df |> 
    select(
      `Location` = location_granular,
      `Job title` = title_general,
      `Job details` = title_detail,
      `Field` = biotech_sub_industry,
      `Company/org` = company_or_institution_name,
      `Salary (base)` = salary_base,
      `Bonus` = bonus,
      `Bonus %` = bonus_pct,
      `Experience (yr)` = years_of_experience,
      `Highest education` = highest_achieved_formal_education,
      
      `Date` = date
    ) |>
    gt::gt() 
}

#' reactable table showing raw data
#' 
#' @param id used for show/hide columns JS 
#' @export
#' @examples
#' salaries |> slice(1:20) |>  rt_table_raw()
rt_table_raw <- function(.df, id = 'table-raw', ...) {
  .df_select <- .df |> 
    mutate(
      base_bonus = glue::glue(
        "${.base}K | {.bonus}K ({.bonus_p})",
        .base = round(salary_base, -3)/1000, .bonus = round(bonus, -3)/1000,
        .bonus_p = scales::percent(bonus_pct, accuracy = 1)
        
      )
    ) |> 
    mutate(
      html_company_location_date = glue::glue(
        "{tidyr::replace_na(company_or_institution_name, '-')}<br>",
        '<p style="font-size:0.6rem;color:grey;margin:0">{location_granular} | {date}</p>'
      ),
      
      html_total_base_bonus = glue::glue(
        "{scales::dollar(salary_total)}<br>",
        '<p style="font-size:0.6rem;color:grey;margin:0">{base_bonus}</p>'
      ),
      
      html_title_detail_yoe = glue::glue(
        "{tidyr::replace_na(as.character(title_detail), '-')}<br>",
        '<p style="font-size:0.6rem;color:grey;margin:0">',
        '{years_of_experience} years',
        '</p>'
      )
    ) |> 
    select(
      html_company_location_date,
      html_total_base_bonus,
      html_title_detail_yoe,
      Stock = compensation_annual_equity,
      `Field` = biotech_sub_industry,
      `Highest education` = highest_achieved_formal_education,
      
    )
  .df_select |> 
    reactable_rbs(
      ...,
      #elementId = id,
      columns = list(
        html_company_location_date = reactable::colDef(
          html = TRUE,
          name = 'Company<br><span style="font-size:0.6rem;color:grey">Location | Date</span>',
          minWidth = 120
          ),
        html_total_base_bonus = reactable::colDef(
          html = TRUE,
          name = 'Total<br><span style="font-size:0.6rem;color:grey">Base | Bonus</span>',
          minWidth = 100
        ),
        html_title_detail_yoe = reactable::colDef(
          html = TRUE,
          name = 'Title<br><span style="font-size:0.6rem;color:grey">Years of Experience</span>',
          minWidth = 80
        ),
        
        # hidden by default:
        `Field` = reactable::colDef(show = FALSE),
        `Stock` = reactable::colDef(show = FALSE),
        `Highest education` = reactable::colDef(show = FALSE)
      )
    )
}

#' @export
reactable_rbs <- function(.df, ...) {
  .df |> 
    reactable::reactable(
      ...,
      highlight = TRUE,
      resizable = TRUE,
      fullWidth = TRUE,
      wrap = TRUE,
      language = reactable::reactableLang(
        searchPlaceholder = "Search"
      ),
      showSortable = TRUE,
      theme = reactable::reactableTheme(
        style = list(
          paddingLeft = "0px",
          paddingRigt = "0px",
          marginTop = '0px',
          color = "#EEE8D5",
          backgroundColor = "#222627",
          fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif",
          fontSize = "0.8rem"
        ),
        highlightColor = "#393C3D",
        borderWidth = '0px',
        headerStyle = list(
          color = 'seagreen'
        ),
        searchInputStyle = list(
          paddingLeft = "8px",
          paddingTop = "6px",
          paddingBottom = "6px",
          borderRadius = '4px',
          display = 'inline-block',
          align = 'left',
          borderColor = 'transparent', #'#EEE8D5',
          borderWidth = '2px',
          #border  = '2px',
          backgroundColor = 
            '#202020',
          #'#222627',
          highlightColor = "#393C3D",
          width = "100%",
          
          backgroundRepeat = "no-repeat",
          "&:focus" = list(backgroundColor = "#393C3D", border = "none"),
          "&:hover, &:focus" = list(backgroundColor = '#393C3D', color = '#EEE8D5'),
          "::placeholder" = list(color = 'grey'),
          "&:hover::placeholder, &:focus::placeholder" = list(color = '#EEE8D5')
        )
      )
    )
}