# https://github.com/rocker-org/rocker-versioned2/wiki/shiny-verse_9dec38cb85d0
FROM rocker/tidyverse:4.3.3

# Install clusterProfiler and related packages for GSEA
RUN install2.r --error --skipinstalled \
    here \
    bslib \
    bsicons \ 
    gt \
    gtExtras \
    paletteer \
    plotly \
    DT \
    htmlwidgets \
    skimr

# dev version reactable for server side
RUN installGithub.r glin/reactable \
    && rm -rf /tmp/downloaded_packages/

# copy files
WORKDIR /home/app/
COPY . .

# create data
RUN R -e "quarto::quarto_render('inst/extdata/clean_data.qmd')"

# install app
RUN R -e "devtools::install()"

# user
RUN useradd -ms /bin/bash shiny
RUN chmod -R 777 /home/app
USER shiny

# run the app
EXPOSE 3838
CMD ["R",  "--vanilla", "-e", "rbiotechsalary::launch_app(port = 3838, host = '0.0.0.0')"]