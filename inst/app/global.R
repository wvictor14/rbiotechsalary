library(shiny)
library(bslib)
library(dplyr)
library(rbiotechsalary)
library(htmltools)

# can read locally if needed
#salaries_file <- fs::path_package(
#  'rbiotechsalary', 'extdata', 'salary_results_cleaned.csv'
#)
message('loading data from github')
salaries_file <- 'https://raw.githubusercontent.com/wvictor14/rbiotechsalarydata/refs/heads/main/salary_results_cleaned.csv'
salaries <- readr::read_csv(salaries_file, show_col_types = FALSE) |>
  rb_clean_data()
message('processed data loaded')

# links
link_add_data <- shiny::tags$a(
  bsicons::bs_icon("plus-lg"),
  " Add Your Salary",
  href = "https://docs.google.com/forms/d/e/1FAIpQLSdj_9sP4DRp_3OuKsOT0wWQBTCHePo-CdBQOso8mvQJks4rIQ/viewform",
  target = "_blank"
)
link_github <- shiny::tags$a(
  shiny::icon("github"),
  " Source",
  href = "https://github.com/wvictor14/rbiotechsalary",
  target = "_blank"
)
link_reddit <- shiny::tags$a(
  bsicons::bs_icon("reddit"),
  " Reddit",
  href = "https://www.reddit.com/r/biotech/comments/125cy06/rbiotech_salary_and_company_survey/",
  target = "_blank"
)
link_google <- shiny::tags$a(
  bsicons::bs_icon("file-earmark-spreadsheet"),
  " Raw Data",
  href = "https://docs.google.com/spreadsheets/d/1G0FmJhkOME_sv66hWmhnZS5qR2KMTY7nzkxksv46bfk/edit?usp=sharing",
  target = "_blank"
)

# colors
.colors <- list(
  'primary' = '#41AB5DFF'
)

version <- paste0('v', as.character(utils::packageVersion('rbiotechsalary')))

# companies
select_companies_choices <- salaries |>
  count(company_or_institution_name) |>
  arrange(desc(n)) |>
  filter(
    !is.na(company_or_institution_name),
    company_or_institution_name != 'Prefer not to say',
    n > 1
  ) |>
  pull(company_or_institution_name)

stringr::str_subset(colnames(salaries), 'company')

companies <- salaries |>

  # harmonize the reviews
  distinct(
    company_or_institution_name,
    optional_company_review,
    provide_a_review_and_rate_your_company_institution_and_experience,
    which_of_the_following_best_describes_your_company,
    company_detail_approximate_company_size,
    company_details_public_private_start_up_subsidiary_of
  ) |>
  mutate(
    company_review = coalesce(
      optional_company_review,
      provide_a_review_and_rate_your_company_institution_and_experience
    )
  ) |>
  relocate(company_review, .before = optional_company_review) |>
  select(
    -optional_company_review,
    -provide_a_review_and_rate_your_company_institution_and_experience
  ) |>

  # harmonize the public / private field
  mutate(
    public_or_private = coalesce(
      which_of_the_following_best_describes_your_company,
      company_details_public_private_start_up_subsidiary_of
    )
  ) |>
  relocate(
    public_or_private,
    .before = which_of_the_following_best_describes_your_company
  ) |>
  select(
    -which_of_the_following_best_describes_your_company,
    -company_details_public_private_start_up_subsidiary_of
  )
