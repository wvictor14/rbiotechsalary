

# https://github.com/rocker-org/rocker-versioned2/wiki/shiny-verse_9dec38cb85d0
FROM rocker/shiny-verse:4.3.3

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

# copy app files to shiny server folder
RUN rm -rf /srv/shiny-server/sample-apps
COPY inst/application/* /srv/shiny-server/rbiotechsalary/

# run the app
USER shiny
EXPOSE 3838
CMD ["/usr/bin/shiny-server"]