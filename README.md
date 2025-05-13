## Distibuted Data Networks Visualization Tool

Distributed data networks (DDNs) and multi-database studies offer a new way of conducting health research that is rapid, rigorous, and reproducible. 
However, the study populations contained in each database may be heterogeneous in terms of event rates and confounding structures.

There are two main purposes to this project: 
1. Develop a web application tool to visualize data for multi-database studies using as an example COVID-19 data from the United Kingdom, British Columbia, and Ontario for different new-user active comparator cohorts. 
2. Make this tool accessible and adaptable for other researchers by providing a data creation guide and modular code for the app.

You can check out the live tool at: https://gwenaubrac.shinyapps.io/host_ddn_app/

### 1. Developing the web application

To do so, we used CPRD data on different cohorts to generate data describing the rate of events, the prevalence of confounders, and the association between confounders and the event of interest over time. 
At this stage, we only have CPRD data, but this data will be combined with results from other data sources. 
The interactive tool will display features of the populations in each database to help researchers identify key differences between them at a glance. 
Access to CPRD data is supported by ISAC protocol 24_004042. This work is conducted at the Lady Davis Institute of the Jewish General Hospital in Montreal.

### 2. Making the tool accessible

You can use this code to run your own web application that displays data from different regions. 
This can be useful for other DDN studies, but also any multi-database study or mutli-site clinical trial where you want to compare data from different sources. 
Here are the steps you need to take to do so: 
- Download R and RStudio if you haven't already. 
- Follow our guide (sample_code) to set up folders and create tables that describe your data, which will be displayed by the app. 
- Download or copy paste the source code from our app in three separate files in your own project folder: ui.R, server.R, and global.R. 
- In global.R, manually edit sections that are specific to your project (such as name of the tool, name of the cohorts and treatment groups, list of outcomes evaluated, etc.). The instructions on how to edit this are directly included in the global.R file.
- Run the application locally and publish it if you want. Please refer directly to instructions from Shiny R on how to do this.

### 3. What's contained in this github page

| Folder | Content | Description |
|-------------------------|------------------------|------------------------|
| app | data, rsconnect, code for the web application | Here you can find the server.R, ui.R, and global.R files to run the application. You can reuse this code (copy it) and just modify the relevant information in global.R for your own purposes. |
| generate-data | code to create tables that are displayed in the web application | This is the code used in our project to generate the data tables displayed by the app for the CPRD region only. |
| sample | toy data and reproducible guide to create tables displayed in the web application | The most challenging part with using this tool is actually to create the data that it displays in the proper format. This is why we made a reproducible guide that takes you through each step. |
