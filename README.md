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

? indicate still considering if worth

### General

branch `filter-to-sci-track` is to experiment using only cleaned data (scientist, director, VP)

-   [ ] add research associate data
-   [ ] add graphs
    -   [ ] salary by country
    -   [x] salary by experience

### For cleaning

Targeted minimum set of variables:

-   [x] timestamp
-   [x] role title
-   [x] location
-   [x] salary base
-   [x] salary bonus
-   [x] experience years
-   [ ] experience degree

### For app

**Filters**

-   [x] add filter by location
-   [x] add filter by timestamp
    -   [ ] ?select by year, or by all
-   [x] add filter by role title
-   [ ] ?filter by background experience (\# of years, degree)

**Salary graphs**

How to lay these out? by tabs?

-   [ ] salary x experience
-   [x] histogram
    -   [ ] add bonus / TC

    -   [ ] show median in text + line

    -   [ ] show range (10th and 90th percentile?)

**Table**

-   [ ] gt output for most customized output?
-   [ ] show TC and breakdown base and bonus concisely

# docker

```bash
sudo docker container ls
sudo docker container run -d  -p 3838:3838 mdancho/shinyauth
sudo docker container exec -it [CONTAINER] bash



docker build -t rbiotechsalary .
docker run -p 3838:3838 rbiotechsalary
```