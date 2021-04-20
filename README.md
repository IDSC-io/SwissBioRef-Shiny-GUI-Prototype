# Swiss BioRef R Shiny Prototype Application

Project for the estimation of comparable reference values in Clinical Medicine using a user-facing graphical user interface (GUI). This prototype application was developed as a Bioinformatics Master Thesis.

**Author**: Tobias Blatter  
**Supervisor**: Prof. Alexander Leichtle

## Project Description

Comparison of test results is an important part of clinical medicine. Clinicians use quantitative standards for normality to assess how a clinical test result may be different or abnormal. The reference values are estimated from test results of a healthy population. To form these populations, cohort data is retrieved using structured query language (SQL). In order to analyze these SQL query results, several strategies for both query automation and interactive reporting with R-evaluation exist. 

This Bioinformatics thesis aimed at developing and deploying an R Shiny application, which is able to read specific data from an SQL query data frame and handle the statistical analysis of reference value estimation in a responsive and reactive GUI. The developed application allows the user to adjust the parameters of the analysis interactively with the use of intuitive widgets. This full application prototype is able to estimate reference values from a data set of two million data points. Various requirements for a secure and reliable clinical application have been implemented in the provided application prototype.

We developed this R Shiny prototype application as part of the research project “Swiss BioRef”. The goal of the “Swiss BioRef” research project is the establishment of an infrastructure to generate more accurate reference values using laboratory data from hospitals and study cohorts. The “Swiss BioRef” research project is funded by the Swiss Personalized Health Network (SPHN). An application for clinical use is currently being developed.


## Prerequisites

* R >= 3.8
* R Shiny >= 1.5.0
* R Shiny Server >= 1.5 or RStudio Desktop >= 1.2

The GUI can be run on Shiny server or on the desktop version of RStudio. The parameter `config.onServer` in the `config.R file` has to be adjusted accordingly.

## Setting up

Before the first initiation of the application, parameters have to be adjusted in the `config.R` file e.g. Login credentials, Rlib path, data path and data name.

## Including your own data

The data is loaded from a local `.csv` file. This prototype includes a synthetic mock up data set. To include your own data, columns and column names of your data frame should conform to the provided mock up data. The data should be put the folder `/data`. 

This application is able to generate reference values from 22 hematological and biochemical blood analytes. Various query settings can be interactively adjusted in the application and after pressing the button "Run analysis".  







