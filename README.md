# PredictiveValueFPs
=============================================================================

A repository for a project to examine the predictive value of frequent patterns. 



<img src="https://img.shields.io/badge/Study%20Status-Results%20Available-yellow.svg" alt="Study Status: Results Available">

- Analytics use case(s): **Population-Level Estimation**
- Study type: **Clinical Application**
- Tags: **-**
- Study lead: **S.Ioannou**
- Study start date: **10 July 2022**
- Study end date: **-**
- Protocol: **add protocol**
- Publications: **-**
- Results explorer: **-**

Requirements
============

- A database in [Common Data Model version 5](https://github.com/OHDSI/CommonDataModel) in one of these platforms: SQL Server, Oracle, PostgreSQL, IBM Netezza, Apache Impala, Amazon RedShift, Google BigQuery, or Microsoft APS.
- R version 4.0.5
- On Windows: [RTools](http://cran.r-project.org/bin/windows/Rtools/)
- [Java](http://java.com)
- 100 GB of free disk space

How to run
==========
1. Follow [these instructions](https://ohdsi.github.io/Hades/rSetup.html) for setting up your R environment, including RTools and Java.

2. Open your study package in RStudio. Use the following code to install all the dependencies:

	```r
	install.packages("renv")
	renv::activate()
	renv::restore()
	```

3. In RStudio, select 'Build' then 'Install and Restart' to install the `PredictiveValueFPs` package.

4. Before running the analysis, fill in the missing parts of the code provided under `extras/CodeToRun_parallel.R` and `extras/ScriptToRunParallel.R`. Then run the contents of the `extras/CodeToRun_parallel.R` to run the analysis. 

License
=======
The `PredictiveValueFPs` package is licensed under Apache License 2.0

Development
===========
`PredictiveValueFPs` was developed in ATLAS and R Studio.

### Development status

Manuscript preparation. 
