#' table_raw ui
#' @export
table_raw_ui <- function(id, ...) {
  tagList(
    gt::gt_output(NS(id, "table_raw", ...))
  )
}

#' table_raw server 
#' @export
table_raw_server <- function(id, .salaries, ...) {
  stopifnot(is.reactive(.salaries))
  moduleServer(id, function(input, output, session) {
    
    output$table_raw <- gt::render_gt(
      ...,
      expr  = {
        .table <- if (nrow(.salaries()) > 0) {
          selected_cols <- .salaries() |>
            arrange(desc(date)) |>
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
            
            slice(1:20) |> 
            gt::gt() |> 
            gt::opt_interactive()
          selected_cols
        } else {
          tibble() |>  gt::gt()
        }
      }
    )
  })
}