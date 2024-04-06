#' table_raw ui
#' @export
table_raw_ui <- function(id, ...) {
  tagList(
    #gt::gt_output(NS(id, "table_raw", ...))
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
      theme = reactable::reactableTheme(
        
        # Dark mode theme 
        style = list(
          color = "#EEE8D5",
          backgroundColor = "#32393F",
          fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif"
        ),
        highlightColor = "#1D262E",
        cellPadding = "0px 0px",
        searchInputStyle = list(
          borderColor = '#EEE8D5',
          borderWidth = '0.5px',
          backgroundColor = '#32393F',
          width = "25%"
        )
      )
    )
}