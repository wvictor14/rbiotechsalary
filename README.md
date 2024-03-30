# rbiotechsalary

<!-- badges: start -->

<!-- badges: end -->

## Installation

You can install the development version of rbiotechsalary from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("wvictor14/rbiotechsalary")
```

## Ideas

? indicate still considering if worth

### deployment

- [x] dockerize
- [x] decide on hosting service - digital ocean
- [ ] deploy 
- [ ] automate software updates
- [ ] automate data updates
    - [ ] target pipeline
    - [ ] automate new data cleaning and uploading

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

for remembering how to docker:

```bash
docker container ls
docker container run -d  -p 3838:3838 mdancho/shinyauth
docker container exec -it app1 bash
cd /var/log/shiny-server/

docker build -t rbiotechsalary .
docker push victor2wy/rbiotechsalary # push to dockerhub

docker run --user shiny -p 3838:3838  --name app1 rbiotechsalary # local
docker run -dp 3838:3838 victor2wy/rbiotechsalary #dockerhub
```
