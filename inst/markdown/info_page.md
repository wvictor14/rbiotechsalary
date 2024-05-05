My primary motivation for this project was to explore certain technical tools and concepts that I've been interested in exploring for some time. I figured I would try to make the resulting product useful and something I could return to over time  for more sandboxing activities. I also expect to use on this resource in future job searching experiences, so hoping that this product will benefit a future me.

Tools and concepts I explored:

- bslib (:thumbs-up:)
- shiny.fluent (no thanks)
- css/html/js (dang, latter two are annoying)
- devops / docker / self-hosting (did not enjoy, but satisfying to see it come together)
- optimizing shiny app structure for deploy

# Goals of this project

Though there are several Salary sites to explore salary data (e.g. [Glassdoor](https://www.glassdoor.com/Salaries/index.htm), [Salary.com](https://www.salary.com/)), it is sometimes unclear how reliable that data is. For example, Glassdoor company ratings and reviews fluctuate often and sometimes dramatically. Larger companies often will task HR to [remove negative reviews](https://blog.pragmaticengineer.com/layoffs-push-down-scores-on-glassdoor/) on Glassdoor; read more about shady Glassdoor practices [here](https://www.wearedevelopers.com/magazine/does-glassdoor-remove-negative-company-reviews).  A second limitation of these salary aggregator sites is that they are non-specific and it can be sometimes difficult to find data specific for biotech/pharma. 

Another source of salary data is from reddit [r/biotech](https://www.reddit.com/r/biotech/comments/125cy06/rbiotech_salary_and_company_survey/), which I found useful in my own job search experiences. In contrast to the salary site aggregators mentioned above, the data is highly transparent and also specific to biotech/pharma. One of the biggest limitations of this data though, is the lack of standardization in, for example, job titles and location (e.g. these should probably be the same: *bay area*, *SF bay area*, *sf*, *san francisco*) and lack of data QC to prevent and remove poor quality data (forgetting a 0 in salary $). For these reason, querying the biotech salary data is cumbersome and limiting.

 *rbiotechsalary* is a data project that includes data ingestion, cleaning, normalization, aggregation, summarization and visualization to reddit r/biotech survey data. 
 
Project components:

1. **data ingestion** data is automatically ingested every time the app image is built and deployed. data source: [googlesheet](https://docs.google.com/spreadsheets/d/1G0FmJhkOME_sv66hWmhnZS5qR2KMTY7nzkxksv46bfk/edit#gid=491268892)
2. **data cleaning** data cleaning is a significant effort and only done for selected prioritized data variables. This step is coded in form of a quarto markdown script that is hosted on github, and runs after step 1. automatically ([source](https://github.com/wvictor14/rbiotechsalary/blob/main/inst/extdata/clean_data.qmd), [rendered html](https://github.com/wvictor14/rbiotechsalary/blob/main/inst/extdata/clean_data.html)])
3. **public web-app** a shiny app provides web-based data aggregation and visualization. code for shiny app [here](https://github.com/wvictor14/rbiotechsalary)

# Challenges

Data is highly unnormalized. At some point the survey changed, so half the data is significantly different from the other half. 

Data is not optimally standardized. Some variables that shouldn't be free form are free-form, making at a lot of work to normalize the data and also to correct / filter errors.

These challenges are ultimately why I chose to focus on a an initially subset of the data and prioritized certain data variables over others. I focused on Scientist track, but neglected manufacturing, clinical, and probably others.

# Contributions, Feedback, and Contact

Am open to any type of contribution (e.g. manufacturing, clinical data?), and feedback 

For features, issues, and contributions [here](https://github.com/wvictor14/rbiotechsalary/issues)

