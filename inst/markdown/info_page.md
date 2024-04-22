# Goals of this project

Though there are several Salary sites to explore salary data, it is sometimes unclear how reliable that data is. It is also often difficult to find data specific to jobs in biotech/pharma. 

When I was looking for jobs, I found the survey data on r/biotech to be useful for understanding relevant salary ranges. Though data is not always valid, it is a highly transparent source of salary data. 

The goal of this project was to aggregate, summarize, and visual the salary data from r/biotech and serve these endpoints in the form of an interactive web app.

The other goals of this project was to also play with some tools and concepts I've been interested in e.g. bslib, shiny.fluent, css/html/js, devops/docker/hosting

# Challenges

Data is highly unnormalized. At some point the survey changed, so half the data is totally different from the other half. 

Data is not optimally standardized. Some variables that shouldn't be free form are free-form, making at a lot of work to normalize the data and also to correct / filter errors.

# Data

Data from this project was taken from Reddit [r/biotech post](https://www.reddit.com/r/biotech/comments/125cy06/rbiotech_salary_and_company_survey/).

Data is updated by manually running an [R quarto script](https://github.com/wvictor14/rbiotechsalary/blob/main/R/clean_data.qmd) that pulls from the survey data on [google sheets](https://docs.google.com/spreadsheets/d/1G0FmJhkOME_sv66hWmhnZS5qR2KMTY7nzkxksv46bfk/edit#gid=491268892)

There is a lot of data that I have processed but data there is a ton of data available that can be added, such as

- roles (dev, clin, manufacturing)
- field (small molecular / antibody etc.)
- variables (stock, granular location data)

# Contributions, Feedback, and Contact

Am open for help in any way =)

Easy way to contribute is to add your salary to the [survey](https://docs.google.com/forms/d/e/1FAIpQLSeXpPvkjdf8EPRU_cAdyMAUFVjgx67nyVgDxV7TNHeFkR2k9A/viewform)

For features, issues, and contributions please @ me [here(https://github.com/wvictor14/rbiotechsalary)

