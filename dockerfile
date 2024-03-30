

# https://github.com/rocker-org/rocker-versioned2/wiki/shiny-verse_9dec38cb85d0
FROM rocker/shiny-verse:4.2.3

# Install clusterProfiler and related packages for GSEA
RUN install2.r --error --skipinstalled \
    shiny.fluent \
    gt \
    gtExtras \
    paletteer \
    plotly 
RUN R -e "devtools::install_github('wvictor14/r_biotech_salary')"

# copy app files
RUN rm -rf /srv/shiny-server/sample-apps
COPY inst/application/* /srv/shiny-server/rbiotechsalary

USER shiny

EXPOSE 3838

CMD ["/usr/bin/shiny-server"]