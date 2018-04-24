---
title: "README"
author: "Jamaal Green"
date: "September 7, 2017"
---

# lehdr

**lehdr** is an R package that allows users to interface with the [Longitudinal and Employer-Household Dynamics (LEHD)](https://lehd.ces.census.gov/) Origin-Destination Employment Statistics (LODES) dataset returned as dataframes. The package is currently in development and can be installed at:

## Installation

```{r}
install.packages("devtools")
devtools::install_github("jamgreen/lehdr")
```

## Usage

```{r}
library(lehdr)

grab_lodes(state = 'or', year = 2014, lodes_type = "od", job_type = "JT01", 
           segment = "SA01", state_part = "main", tract = TRUE)
           
grab_lodes(state = c('or', 'ri'), year = c(2013, 2014), lodes_type = "od", job_type = "JT01", 
           segment = "SA01", state_part = "main", tract = TRUE)           
```

# Why lehdr?

The LODES dataset is frequently used by transportation and economic development planners, regional economists, disaster managers and other public servants in order to have a fine grained understaning of the distribution of employment. Such data is integral for regional travel demand models that help to dictate transportation policy options, regional economists and economic development planners interested in the spatial distribution of particular kinds of work use the data to weigh different industrial or workforce policy options. Finally, as a census product, the LODES data can be joined to census Decennial or American Community Survey data to help visualize the interactions between different population groups and work. In short, the LODES dataset is the only source of detailed geographic information on employment for the country and should be more widely available for researchers and analysts who work on regional development issues. 

# Future Development

Currently, **lehdr** is designed to grab the LODES flat files (origin-destination, workplace, and residential association files) and includes an option to aggregate results to the census tract level for analysts who find the fuzzing at the block level too great. 

Next steps include exploring integration of the package with the [**sf**](https://cran.r-project.org/web/packages/sf/index.html) and [**tigris**](https://cran.r-project.org/web/packages/tigris/index.html) packages to allow for easier mapping of LODES data. 

#Acknowledgements

This package would not exist in its current format without the inspiration of [Bob Rudis's](https://rud.is/b/) [lodes package](https://github.com/hrbrmstr/lodes)

# Build Status

[![Build Status](https://travis-ci.org/jamgreen/lehdr.svg?branch=master)](https://travis-ci.org/jamgreen/lehdr)