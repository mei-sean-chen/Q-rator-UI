# Q-rator-UI Beta Release readMe

Q-rator is an R lang. script that automates creation of files for FlexQTL and other genetic datasets, written for R version 4.0.5+. It requires several input files (which must be converted to comma-separated value files before running) and creates several output files, which we will enumerate below.  

## Setup

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

## Required Files

**Map file:** intmap11_20k 

**Marker data:** 20k_8koverlap 

**Phenotype file:** The formatting of your phenotype data must follow these rules. List all individuals in column A with column name “Index”. Data for each individual goes in columns B and onward. Each column of phenotype data must have a descriptive column name in Row 1. Pictured is an example phenotype data file.  

## Features & Nav

Navigate Q-rator by the upper navbar. The tabs are organized in sequential order.  

Upload the required files to File Input tab.  

Pick assembly settings at Config. tab and load them with the 'Load Config' button on the sidebar. All settings are explained in detail in the Glossary.  

Verify applied settings by checking the data summary tables. You can pick which table to render in the main panel via the dropdown menu titled “Display Helper Tables”. All summary tables are explained in detail in the Glossary.  

Once you have confirmed your settings, provide a descriptive name for your current session of Q-rator and click the “Export” button. All necessary files will be exported with file names based on the session name you provided. Files are explained in detail in the Glossary.  

# Glossary

## Settings

**Resolution (cM):** This slider controls the resolution of your map. Provide an input integer between 1 and 10, and this integer becomes the scale of your map. For instance, setting the slider to 5 cM will subset markers every 5 cM beginning with the first marker in group 1. Setting the slider to 0 is the max resolution and will keep every marker in your map. Default value is 0.  

**Remove homozygous markers:** Set this flag to filter out all markers for which all parents of your input individuals are homozygous (AA, BB, CC, etc).  

**Remove by marker location:** Set this flag to filter out markers according to these rules: if more than one marker shares the same locus within a Group, the first marker in the map is kept in the data set, and the other(s) is/are filtered out.  

**Phenotype data:** This group of flags determines which columns of phenotype data you wish to keep in the output data file.  

**Add markers:** Correct any previous subsets on the markers in your data set. If you have removed a marker from the data set by applying a setting, you may add it back into the data set by providing its ID in the text field. To apply the addition, press the ‘Load Config.’ button.  

**Remove markers:** If you wish to remove a marker in your data set, provide its ID in the text field. To apply the removal, press the ‘Load Config.’ button.  

**Session ID:** Provide a descriptive name for the current session of Q-rator. The provided text will be used to name all exported files.  

## Summary Tables

**Errors** is a list of all individuals that were present in the input phenotype data file, but not present in the 20k_8koverlap file. If there were no clerical errors in the creation of your phenotype data file, then it may well be empty 

**Parent-homozygous Markers** is a subset of the master data set which shows the set of all parents of input individuals, and all markers for which the parents are homozygous.  

**Marker summary (Parents)** is a data frame showing allele frequencies for every marker calculated for the set of all parents of input individuals.  

**Marker summary (Input Set)** is a data frame showing allele frequencies for every marker calculated for the set of all input individuals, group and locus for every marker, and whether or not the marker is included in your final data set depending on the settings you applied. Column “Included” takes binary values: 1 if the marker is included in the data set, 0 if it has been removed.  

**Positive Marker Set** is a subset of the input set marker summary. Only rows which have value of 1 in the “Included” column are present. If default settings are applied, then this set is equal to the marker summary input set.  

**Negative Marker Set** is a subset of the input set marker summary. Only rows which have value of 0 in the “Included” column are present. If default settings are applied, then this data frame is empty. 

## Exported Files

Note: these are generic names. If you provided a session ID under the settings, it will be applied to all the exported files. For instance, if the session ID is “test1”, then the errors file will be named `test1_errors.csv`, the dat file will be called `test1_dat.csv` and so on. 

`errors.csv` is an exported version of the errors table.  

`dat.csv` is the sheet to be used in FlexQTL, matching individuals to their pedigrees, phenotype data, and marker data. Must be converted manually to DAT as it is a CSV by default.  

`map.csv` is the map file corresponding to `dat.csv`, with those SNPs present in dat.csv represented in column A, and their Group and location in column B. Must be converted to a .map file as it is a CSV by default.  

`marker_count.txt` provides a count of SNPs per group based on the SNPs present in `dat.csv`. Row number corresponds to group number, so the integer on row 3 is the count of group 3 SNPs present in `dat.csv`.  
