mosquito-Rajshahi
=================

# <span xmlns:dct="http://purl.org/dc/terms/" property="dct:title">mosquito-Rajshahi code repository</span>

This repository contains all files needed to run the analyses in:

> <span xmlns:cc="http://creativecommons.org/ns#" property="cc:attributionName">Lord, JS, HM Al-amin, S Chakma, S Alam, ES Gurley, and JRC Pulliam. Sampling design influences the observed dominance of Culex tritaeniorhynchus: implications for understanding the Japanese encephalitis virus transmission cycle . 

The code is made available under a <a rel="license" href="http://creativecommons.org/licenses/by/4.0/">Creative Commons Attribution 4.0 International License</a>. You are free to reuse this code provided that you give appropriate credit, provide a link to the license, and indicate if changes were made. You may do so in any reasonable manner, but not in any way that suggests the licensor endorses you or your use. Giving appropriate credit includes citation of the above publication and providing a link to this repository:

<a xmlns:dct="http://purl.org/dc/terms/" href="https://github.com/PulliamLab-UFL/mosquito-Rajshahi" rel="dct:source">https://github.com/PulliamLab-UFL/mosquito-Rajshahi</a>

<a rel="license" href="http://creativecommons.org/licenses/by/4.0/"><img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by/4.0/88x31.png" /></a><br />

We suggest you open [this project file](mosquito-Rajshahi.Rproj) file in [R Studio](rstudio.org). The files that form part of this project file are detailed below and are split so that different levels of detail can be observed. If you want to view the tables and figures only then run 
- [**tables.write**](tables.write.R) and  [**figures.R**](figures.R). The manuscript tables have already been written to file and can be found in the repository. The raw data is also available as an SQLite database file.  If you want to see further levels of detail we suggest that you view and run the files in the following order:
- [**db_queries.R**](db_queries.R)
- [**data_prep.R**](data_prep.R) 
- [**tables.write**](tables.write.R)
- [**prop.ci**](prop.ci.R)
- [**hills.d.R**](hills.d.R)
- [**fig1.R**](fig1.R)
- [**fig2.R**](fig2.R)
- [**fig3.4.R**](fig3.4.R)
- [**fig5.R**](fig5.R)
- [**table4.R**](table5.R)

These files plus the database are explained briefly below:

## Data
- **Moz.db** - this sqlite database file contains the raw data. It contains data for the eight villages included in analyses for the manuscript in addition to two additional villages for which there are no household host data.

## Data preparation
- **db_queries.R** - queries Moz.db and retrieves the required data
- **data_prep.R** - uses the query results from db_queries.R and reshapes the data into the required formats

## Tables
- **tables.write** - creates and writes all the manuscript tables to file

## Figures
- **figures.R** - produces all the manuscript figures

## Analyses
- **prop.ci.R** - calculates confidence intervals for proportion data for table 1
- **hills.d.R** - calculates Hills Diversity Numbers for table 2
- **table4.R** - produces the mosquito species abundance data required for table 4
- **fig1.R** - species accumulation curve for figure 1
- **fig2.R** - log odds ratio test for figure 2
- **fig3.4.R** - linear regression for figures 3 and 4 and table 3
- **fig5.R** - produces the mosquito species proportions required for figure 5
