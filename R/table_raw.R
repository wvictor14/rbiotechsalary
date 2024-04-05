#' table_raw ui
#' @export
table_raw_ui <- function(id, ...) {
  tagList(
    gt::gt_output(NS(id, "table_raw", ...))
  )
}

#' table_raw server 
#' @export
table_raw_server <- function(id, .salaries, .slice = 1:20, ...) {
  stopifnot(is.reactive(.salaries))
  moduleServer(id, function(input, output, session) {
    
    output$table_raw <- gt::render_gt(
      ...,
      expr  = {
        if (nrow(.salaries()) == 0) return(NULL)
        .salaries() |>
          arrange(desc(date)) |>
          slice(.slice) |> 
          gt_table_raw() |>  
          gt_dark_mode() |> 
          gt::opt_interactive()
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