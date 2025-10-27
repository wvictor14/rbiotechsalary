#' @export
rb_clean_data <- function(.salaries) {
  USA <- c(
    "Pharma Central (NY, NJ, PA)",
    "New England (MA, CT, RI, NH, VT, ME)",
    "DC Metro Area (DC, VA, MD, DE)",
    "Carolinas & Southeast (From NC to AR, South FL and LA)",
    "Midwest (From OH to KS, North to ND)",
    "South & Mountain West (TX to AZ, North to MT)",
    "West Coast (California & Pacific Northwest)",
    "Other US Location (HI, AK, PR, etc.)"
  )

  .salaries |>
    mutate(
      title_general = title_general |>
        forcats::fct() |>
        forcats::fct_relevel(
          'Research Associate',
          'Senior Research Associate',

          'Associate Scientist',
          'Scientist',
          'Senior Scientist',
          'Principal Scientist',

          'Associate Director',
          'Director',
          'Senior Director',
          'Executive Director',

          'VP',
          'SVP'
        ),

      title_category = title_category |>
        forcats::fct() |>
        forcats::fct_relevel(
          'Research Associate',
          'Scientist',
          'Director',
          'VP'
        ),

      location_granular = forcats::fct(location_granular) |>
        forcats::fct_relevel(USA),

      experience_highest_degree = forcats::fct(experience_highest_degree) |>
        forcats::fct_na_value_to_level()
    )
}
