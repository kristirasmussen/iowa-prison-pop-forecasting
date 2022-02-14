# iowa-prison-pop-forecasting

### Description:  
This project aims to forecast Iowa Prison Population levels over the next 10 years at the offense classification and offense type granularity. 
All data used within this project is publicly available from [https://data.iowa.gov/](https://data.iowa.gov/). 
All code is written in the R programming language and, due to packages used within our project, requries a version of R greater than 4.0. 
This collection of files follows the standard R Project implmentation documented [here](https://support.rstudio.com/hc/en-us/articles/200526207-Using-RStudio-Projects) and is easy to import locally into RStudio for further inspection and development. The dashboard was created using the Flexdashboard framework, using a Shiny runtime. 

### Installation/Contribution Guide:  
- This repository is currently public and we suggest users fork this project into their own GitHub account and load into RStudio to make changes, then push back to this repository.
- Data is updated on a monthly basis and stored within the "data" folder of this project.

### Execution:  
- In order to run a demo of our dashboard, the existing project code is published to shinyapps.io [here](https://iowa-prison-forecast.shinyapps.io/iowa-prison-pop-forecasting/)
- Alternatively, if a user wishes to run the dashboard locally from RStudio, users simply need to open the index.Rmd file and "Run Document" from the RStudio user interfact to render the dashboard.
