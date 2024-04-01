# https://github.com/rocker-org/rocker-versioned2/wiki/shiny-verse_9dec38cb85d0
FROM rocker/tidyverse:4.3.3

# Install clusterProfiler and related packages for GSEA
RUN install2.r --error --skipinstalled \
    shiny.fluent \
    gt \
    gtExtras \
    paletteer \
    plotly \
    DT

# install app
WORKDIR /home/app/
COPY . .
RUN R -e "devtools::install()"

# user
RUN useradd -ms /bin/bash shiny
USER shiny

# run the app
EXPOSE 3838
CMD ["R", "-e", "shiny::runApp('/home/app/inst/application/', port = 3838, host = '0.0.0.0')"]