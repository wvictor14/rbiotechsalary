# https://github.com/rocker-org/rocker-versioned2/wiki/shiny-verse_9dec38cb85d0
FROM rocker/tidyverse:4.4.0

# Install R packages 
RUN install2.r --skipinstalled \
    bslib \
    bsicons \
    quarto \
    htmlwidgets \
    here \
    paletteer \
    plotly \
    skimr \
    gt \
    gtExtras 

# dev version reactable for server side
RUN installGithub.r glin/reactable 

# for data cleaning script
RUN install2.r \
    janitor \ 
    tictoc \
    && rm -rf /tmp/downloaded_packages/

# copy files
WORKDIR /home/app/
COPY . .

# install app
RUN R -e "devtools::install()"

# user
RUN useradd -ms /bin/bash shiny
RUN chmod -R 777 /home/app
USER shiny

# run the app
EXPOSE 3838
CMD ["R",  "--vanilla", "-e", "rbiotechsalary::launch_app(port = 3838, host = '0.0.0.0')"]