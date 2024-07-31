
# Plan:

gha to render qmd to html and deploy daily / weekly

#### Problems to plan out:

what are the most important interactive features and how to translate into more limited static html format?


##### 1. Interactivity: filters to users interest
- job title
- country / location

##### 2. Convey key information
- summary statistics e.g. median income
- distribution / variation
- details in table


##### solutions

ojs + plot

- [x] [ojs hover histogram / bar plot](https://observablehq.com/plot/interactions/pointer)
- [x] can ojs be used to convey summary info?
    - idea: precompute across all jd / location?  and display summarized data as table?
    - answer: yes ojs can compute on the fly
- [x] [OJS Filtering can be done](https://quarto.org/docs/interactive/ojs/#example)

filters + plot + table 

# The benefit of all this:

- automatic data updates via gha
- lower overhead 
  - auto deploy via gha to github pages
  - no server needed
    - no cost (no do droplet needed)

## ojs

# Progress

- [x] poc qmd done
- [ ] set up github actions to deploy
- [ ] update reddit post
- [ ] configure domains / hosting
