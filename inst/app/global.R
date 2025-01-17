library(shiny)
library(bslib)
library(dplyr)
library(rbiotechsalary)

# load data for app
skim_raw_data <- load_raw_data() |> 
  skimr::skim() |> 
  as_tibble() |> 
  rename_with(~stringr::str_remove_all(.x, 'character\\.')) |> 
  select(-skim_type, -whitespace) 

message('data summarized with skimr')


salaries_file <- fs::path_package(
  'rbiotechsalary', 'extdata', 'salary_results_cleaned.csv'
)
message('loading data from github')
salaries_file <- 'https://raw.githubusercontent.com/wvictor14/rbiotechsalarydata/refs/heads/main/salary_results_cleaned.csv'
salaries <- readr::read_csv(salaries_file, show_col_types = FALSE) |> 
  rb_clean_data()
message('processed data loaded')

# links
link_add_data <- shiny::tags$a(
  bsicons::bs_icon("plus-lg"), " Add Your Salary",
  href = "https://docs.google.com/forms/d/e/1FAIpQLSeXpPvkjdf8EPRU_cAdyMAUFVjgx67nyVgDxV7TNHeFkR2k9A/viewform",
  target = "_blank"
)
link_github <- shiny::tags$a(
  shiny::icon("github"), " Source",
  href = "https://github.com/wvictor14/rbiotechsalary",
  target = "_blank"
)
link_reddit <- shiny::tags$a(
  bsicons::bs_icon("reddit"), " Reddit",
  href = "https://www.reddit.com/r/biotech/comments/125cy06/rbiotech_salary_and_company_survey/",
  target = "_blank"
)
link_google <- shiny::tags$a(
  bsicons::bs_icon("file-earmark-spreadsheet"), " Raw Data",
  href = "https://docs.google.com/spreadsheets/d/1G0FmJhkOME_sv66hWmhnZS5qR2KMTY7nzkxksv46bfk/edit#gid=491268892",
  target = "_blank"
)

# colors
.colors <- list(
  'primary' = '#41AB5DFF'
)

version <- paste0('v', as.character(utils::packageVersion('rbiotechsalary')))
