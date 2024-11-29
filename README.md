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

1. Provide user-friendly access to cleaned biotech salary information from r/biotech
   -  focus on normalizing the most relevant variables
3. Convey a minimal but relevant set of statistics and visualizations on salary information  
    - enable relevant queries with customized search based on normalized (cleaned) data for role titles, and location data  
4. Promote community involvement and open-source frameworks
    - allow user to explore raw data
    - open-access data provenance 

# Architecture

*Pipeline:*

Google survey -> google sheets -> GHA-powered daily render and publish

*Render and publish:*

- R / Observable JS / quarto backend
- renders static html
- github actions to publish static html to github pages 

*interactivity* is all client-side via Observable JS (ojs)

*data processing* is all using R


# Task list

## Ideas

? indicate still considering if worth

### deployment

- [x] dockerize
- [x] decide on hosting service - digital ocean
- [x] deploy
- [x] automate software updates
- [x] automate data updates
    - [ ] regular pulling updates from google sheets 
    - [ ] target pipeline
    - [x] automate new data cleaning and uploading

### General

-   [x] add research associate data
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
    - [x] raw data - cool
-   [x] histogram
    -   [ ] toggle between base / total
    -   [x] show median in text + line
    -   [x] show range (10th and 90th percentile?)

**Table**

-   [x] reactable winner over gt and DT -> server side processing, feature-ful and looks good
-   [x] show TC and breakdown base and bonus concisely - `merge_cols()`
-   [x] finalize column inclusion
    - [x] options good to include


# docker

for remembering how to docker:

```bash
docker container ls
docker exec -it <container_name> bash

# stop and remove
docker stop $(docker ps -a -q)
docker rm $(docker ps -a -q)

# build and push
docker build -t rbiotechsalary .
docker tag rbiotechsalary victor2wy/rbiotechsalary
docker push victor2wy/rbiotechsalary # push to dockerhub

# run
docker run --user shiny -dp 3838:3838 victor2wy/rbiotechsalary #dockerhub

docker pull victor2wy/rbiotechsalary:latest
docker run --user shiny -p 3838:3838  rbiotechsalary # local
```
