# rbiotechsalary

<!-- badges: start -->

<!-- badges: end -->

## Installation

You can install the development version of rbiotechsalary from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("wvictor14/rbiotechsalary")
```
# goals of this project

1. focus on a core set of features to enable quick development and iteration
2. convey salary information
    -  enable relevant queries with customized search based on normalized (cleaned) data for role titles, and location data
    -  convey salary information concisely through summarized statistics and visualization 
3. Promote community involvement and open-source frameworks
    - allow user to explore raw data

# Task list

## Ideas

? indicate still considering if worth

### deployment

- [x] dockerize
- [x] decide on hosting service - digital ocean
- [x] deploy
- [ ] automate software updates
- [ ] automate data updates
    - [ ] target pipeline
    - [ ] automate new data cleaning and uploading

### General

-   [ ] add research associate data
-   [ ] add other departments like product dev, clinical, business etc.

### For cleaning

Targeted minimum set of variables:

-   [x] timestamp
-   [x] role title
-   [x] location
-   [x] salary base
-   [x] salary bonus
-   [x] experience years

### For app

**Filters**

-   [x] add filter by location
-   [x] add filter by timestamp
    -   [x] select by year, or by all
-   [x] add filter by role title

**Salary graphs**

How to lay these out? by tabs?

-   [x] salary x experience
-   [x] histogram
    -   [ ] toggle between base / total
    -   [ ] show median in text + line
    -   [ ] show range (10th and 90th percentile?)

**Table**

-   [ ] reactable winner over gt and DT -> server side processing, feature-ful and looks good
-   [x] show TC and breakdown base and bonus concisely - `merge_cols()`

# docker

for remembering how to docker:

```bash
docker container ls

# stop and remove
docker stop $(docker ps -a -q)
docker rm $(docker ps -a -q)

# build and push
docker build -t rbiotechsalary .
docker tag rbiotechsalary victor2wy/rbiotechsalary
docker push victor2wy/rbiotechsalary # push to dockerhub

docker pull victor2wy/rbiotechsalary:latest

# run
docker run --user shiny -p 3838:3838  rbiotechsalary # local
docker run --user shiny -dp 3838:3838 victor2wy/rbiotechsalary #dockerhub
```
