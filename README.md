# r_biotech_salary

<!-- badges: start -->

<!-- badges: end -->

## Installation

You can install the development version of r_biotech_salary from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("wvictor14/r_biotech_salary")
```

## Ideas

### For cleaning

Targeted minimum set of variables:

-   [ ] timestamp
-   [ ] role title
-   [ ] location
-   [x] salary base
-   [x] salary bonus
-   [ ] experience years
-   [ ] experience degree

### For app

**Filters**

-   [ ] add filter by location
-   [x] add filter by timestamp
    -   [ ] select by year, or by all
-   [x] add filter by role title
-   [ ] filter by background experience (\# of years, degree)

**Salary graphs**

default now is histogram of salary, consider other graphics, can layout by tabs?

-   [ ] salary x experience

histogram improvements:

-   [ ] above histogram, show average salary, 10th and 90th percentile
-   [ ] include base / tc (base + bonus)

**Table**

-   [ ] gt output for most customized output
-   [ ] show TC and breakdown base and bonus concisely
