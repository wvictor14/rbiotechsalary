
showHideButton = function(id){
  actionButton(
    id,
    "Show/hide additional columns",
    onclick = "Reactable.setHiddenColumns('table-raw', prevColumns => {
        return prevColumns.length === 0 ? ['Job details', 'Field', 'Company/org', 'Highest education'] : []
      })"
  )
}

#' table_raw ui
#' @export
table_raw_ui <- function(id, ...) {
  tagList(
    #gt::gt_output(NS(id, "table_raw", ...))
    showHideButton(NS(id, 'toggle_button')),
    reactable::reactableOutput(NS(id, "table_raw", ...))
  )
}

#' table_raw server 
#' @export
table_raw_server <- function(id, .salaries, .slice = 1:20, ...) {
  stopifnot(is.reactive(.salaries))
  moduleServer(id, function(input, output, session) {
    
    output$table_raw <- reactable::renderReactable(#gt::render_gt(
      ...,
      expr  = {
        if (nrow(.salaries()) == 0) return(NULL)
        
        .salaries() |>
          arrange(desc(date)) |>
          slice(.slice) |>
          rt_table_raw()
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
#' 
#' salaries |> slice(1:20) |>  rt_table_raw()
#' 
rt_table_raw <- function(.df) {
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
    reactable::reactable(
      searchable = TRUE,
      highlight = TRUE,
      elementId = "table-raw",
      
      
      theme = reactable::reactableTheme(
        style = list(
          color = "#EEE8D5",
          backgroundColor = "#222627",
          fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif"
        ),
        highlightColor = "#393C3D",
        borderWidth = '0px',
        #cellPadding = "0px 0px",
        searchInputStyle = list(
          borderColor = 'transparent', #'#EEE8D5',
          borderWidth = '0px',
          backgroundColor = '#393C3D',
          width = "25%"
        )
      ),
      columns = list(
        `Job details` = reactable::colDef(show = FALSE),
        `Field` = reactable::colDef(show = FALSE),
        `Company/org` = reactable::colDef(show = FALSE),
        `Highest education` = reactable::colDef(show = FALSE)
      )
    )
}