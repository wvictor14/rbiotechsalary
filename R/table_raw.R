#' show hide button on reactable
#' 
#' taken from https://github.com/glin/reactable/issues/319
showHideButton = function(id){
  
  actionButton(
    id,
    "Show/hide additional columns",
    onclick = "Reactable.setHiddenColumns('table-raw', prevColumns => {
        return prevColumns.length === 0 ? ['Job title', 'Job details', 'Field', 'Stock', 'Highest education'] : []
      })",
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
    #div(
    #  style = 'display: flex; flex-flow: column wrap;',
      showHideButton(NS(id, 'toggle_button')),
      reactable::reactableOutput(NS(id, "table_raw"), ...)
    #)
  )
}

#' table_raw server 
#' @export
table_raw_server <- function(id, .salaries,  ...) {
  stopifnot(is.reactive(.salaries))
  moduleServer(id, function(input, output, session) {
    
    output$table_raw <- reactable::renderReactable(#gt::render_gt(
      ...,
      expr  = {
        if (nrow(.salaries()) == 0) return(NULL)
        
        .salaries() |>
          arrange(desc(date)) |>
          #slice(.slice) |>
          rt_table_raw(server = TRUE)
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
#' @export
#' @examples
#' salaries |> slice(1:20) |>  rt_table_raw()
rt_table_raw <- function(.df, ...) {
  .df_select <- .df |> 
    mutate(
      base_bonus = glue::glue(
        "${.base}K | {.bonus}K ({.bonus_p})",
        .base = round(salary_base, -3)/1000, .bonus = round(bonus, -3)/1000,
        .bonus_p = scales::percent(bonus_pct, accuracy = 1)
        
      )
    ) |> 
    select(
      `Job title` = title_general,
      `Job details` = title_detail,
      `YOE` = years_of_experience,
      `Total` = salary_total,
      base_bonus,
      Stock = compensation_annual_equity,
      `Company` = company_or_institution_name,
      `Location` = location_granular,
      `Field` = biotech_sub_industry,
      `Highest education` = highest_achieved_formal_education,
      `Date` = date
      
    )
  .df_select |> 
    reactable::reactable(
      ...,
      searchable = TRUE,
      highlight = TRUE,
      resizable = TRUE,
      elementId = "table-raw",
      theme = reactable::reactableTheme(
        style = list(
          marginTop = '0px',
          color = "#EEE8D5",
          backgroundColor = "#222627",
          fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif"
        ),
        highlightColor = "#393C3D",
        borderWidth = '0px',
        #cellPadding = "0px 0px",
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
          borderWidth = '0px',
          border  = 'none',
          backgroundColor = '#222627',
          highlightColor = "#393C3D",
          width = "100%",
          
          backgroundRepeat = "no-repeat",
          "&:focus" = list(backgroundColor = "#393C3D", border = "none"),
          "&:hover, &:focus" = list(backgroundColor = '#393C3D', color = '#EEE8D5'),
          "::placeholder" = list(color = 'grey'),
          "&:hover::placeholder, &:focus::placeholder" = list(color = '#EEE8D5')
        )
      ),
      wrap = FALSE,
      language = reactable::reactableLang(
        searchPlaceholder = "Search Raw Data"
      ),
      showSortable = TRUE,
      columns = list(
        `YOE` = reactable::colDef(
          #style = list(color = 'grey'),
          #headerStyle = list(color = 'grey'),
          width = 60
        ),
        base_bonus = reactable::colDef(
          show = TRUE,
          name = 'Base | Bonus',
          sortable = FALSE,
          width = 145,
          
          style = list(color = 'grey'),
          headerStyle = list(color = 'grey')
        ),
        `Total` = reactable::colDef(
          width = 100,
          format = reactable::colFormat(
            prefix = "$", separators = TRUE, digits = 0
          )
        ),
        
        # hidden by default:
        `Job title` = reactable::colDef(show = FALSE),
        `Job details` = reactable::colDef(show = FALSE),
        `Field` = reactable::colDef(show = FALSE),
        `Stock` = reactable::colDef(show = FALSE),
        #`Company` = reactable::colDef(show = FALSE),
        `Highest education` = reactable::colDef(show = FALSE)
      )
    )
}