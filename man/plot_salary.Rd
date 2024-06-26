% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_salary.R
\name{calculate_salary_stats}
\alias{calculate_salary_stats}
\alias{plot_salary}
\title{plot salary histogram}
\usage{
calculate_salary_stats(.df, x)

plot_salary(.df, x = salary_total, fill = title_general)
}
\arguments{
\item{.df}{salary data}
}
\description{
plot salary histogram
}
\examples{
library(rbiotechsalary)
data(salaries)
salaries_sub <- salaries |>
  dplyr::filter(title_general \%in\% c('Scientist', 'Principal Scientist'),
         stringr::str_detect(location_country, 'United States') )
plot_salary(salaries_sub)

}
