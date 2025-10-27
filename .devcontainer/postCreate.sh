#!bin/bash

R -e "renv::restore(prompt = FALSE)"
R -e "renv::install()"