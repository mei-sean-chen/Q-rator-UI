# Q-rator-UI Beta Release readMe (Luby Lab)

Q-rator is an R lang. script that automates creation of files for FlexQTL and other genetic datasets, written for R version 4.0.5+. It requires several input files (which must be converted to comma-separated value files before running) and creates several output files, which we will enumerate below.  

##Setup

Aside from updating to the latest version of R, Q-rator requires the packages tibble, shiny, shinyFiles, devtools, roxygen2, and of course, the Q-rator package. Install these packages first with the following code: 
```
install.packages("shiny") 
install.packages("devtools") 
install.packages("roxygen2") 
install.packages("tibble") 
```
To install the Q-rator package, first load devtools and so you can use the install_github function. Run the following 2 lines of code.  
```
library(devtools) 
install_github("mei-sean-chen/Q-rator") 
```
Download the Q-rator-UI and unzip contents to the directory of your choice. Navigate to the main directory and open app.R in R Studio. Run the UI with the green play button in the top right corner of the script editor window. The dropdown menu gives the user the option to run the UI in their default web browser, or in an R Studio window. 

##Required Files

**Map file:** intmap11_20k 

**Marker data:** 20k_8koverlap 

**Phenotype file:** The formatting of your phenotype data must follow these rules. List all individuals in column A with column name “Index”. Data for each individual goes in columns B and onward. Each column of phenotype data must have a descriptive column name in Row 1. Pictured is an example phenotype data file.  
