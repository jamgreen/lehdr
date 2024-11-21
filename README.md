README
================
Jamaal Green, University of Pennsylvania; Dillon Mahmoudi, University of
Maryland Baltimore County; Liming Wang, Portland State University

<!-- README.md is generated from README.Rmd. Please edit that file -->

# lehdr

## Build Status

<!-- badges: start -->

[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/lehdr)](https://cran.r-project.org/package=lehdr)

[![R-CMD-check](https://github.com/jamgreen/lehdr/workflows/R-CMD-check/badge.svg)](https://github.com/jamgreen/lehdr/actions)

[![metacran
downloads](https://cranlogs.r-pkg.org/badges/lehdr)](https://cran.r-project.org/package=lehdr)
<!-- commenting out travis
[![Build Status](https://travis-ci.org/jamgreen/lehdr.svg?branch=master)](https://travis-ci.org/jamgreen/lehdr)
--> <!-- badges: end -->

**lehdr** (pronounced: *lee dur* like a metric *litre*) is an R package
that allows users to interface with the [Longitudinal and
Employer-Household Dynamics (LEHD)](https://lehd.ces.census.gov/)
Origin-Destination Employment Statistics (LODES) dataset returned as
dataframes. The package is continually in development and can be
installed via CRAN.

## Installation

You can install the released version of lehdr from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("lehdr")
```

And the development version from [GitHub](https://github.com/) with:

``` r
install.packages("devtools", repos = "http://cran.us.r-project.org")
devtools::install_github("jamgreen/lehdr")
```

## Usage

This first example pulls the Oregon (`state = "or"`) 2020
(`year = 2020`) from LODES version 8 (`version="LODES8"`, default),
origin-destination (`lodes_type = "od"`), primary jobs including private
primary, secondary, and Federal (`job_type = "JT01"`, default), as well as, primary jobs
across ages, earnings, and industry (`segment = "S000"`, default),
aggregated at the Census Tract level rather than the default Census
Block (`agg_geo = "tract"`).

``` r
library(lehdr)
or_od <- grab_lodes(state = "or", 
                    year = 2020, 
                    version = "LODES8", 
                    lodes_type = "od", 
                    job_type = "JT01",
                    segment = "S000", 
                    state_part = "main", 
                    agg_geo = "tract")

head(or_od)
```

The package can be used to retrieve multiple states and years at the
same time by creating a vector or list. This second example pulls the
Oregon AND Rhode Island (`state = c("or", "ri")`) for 2013 and 2014
(`year = c(2013, 2014)` or `year = 2013:2014`).

``` r
grab_lodes(state = c("or", "ri"), 
           year = c(2013, 2014), 
           lodes_type = "od", 
           job_type = "JT01", 
           segment = "S000", 
           state_part = "main", 
           agg_geo = "tract")           
```

Not all years are available for each state. To see all options for
`lodes_type`, `job_type`, and `segment` and the availability for each
state/year, please see the most recent LEHD Technical Document at
<https://lehd.ces.census.gov/data/lodes/LODES8/>.

Using the optional `version` paramater, users can specify which LODES
version to use. Version 8 is default (`version="LODES8"`) is enumerated
at 2020 Census blocks. LODES7 (`version="LODES7"`) is enumerated at 2010
Census blocks, but ends in 2019. LODES5 (`version="LODES5"`) is
enumerated at 2000 Census blocks, but ends in 2009.

Other common uses might include retrieving Residential or Work Area
Characteristics (`lodes_type = "rac"` or `lodes_type = "wac"`
respectively), low income jobs (`segment = "SE01"`) or good producing
jobs (`segment = "SI01"`). Other common geographies might include
retrieving data at the Census Block level (`agg_geo = "block"`, not
necessary as it is default).

## Why lehdr?

The LODES dataset is frequently used by transportation and economic
development planners, regional economists, disaster managers and other
public servants in order to have a fine grained understanding of the
distribution of employment. Such data is integral for regional travel
demand models that help to dictate transportation policy options,
regional economists and economic development planners interested in the
spatial distribution of particular kinds of work use the data to weigh
different industrial or workforce policy options. Finally, as a census
product, the LODES data can be joined to census Decennial or American
Community Survey data to help visualize the interactions between
different population groups and work. In short, the LODES dataset is the
only source of detailed geographic information on employment for the
country and should be more widely available for researchers and analysts
who work on regional development issues.

## Future Development

Currently, **lehdr** is designed to grab the LODES flat files
(origin-destination, workplace, and residential association files) and
includes an option to aggregate results to the census tract level for
analysts who find the fuzzing at the block level too great.

Next steps include exploring integration of the package with the
[**sf**](https://CRAN.R-project.org/package=sf) and
[**tigris**](https://CRAN.R-project.org/package=tigris) packages to
allow for easier mapping of LODES data.

## Acknowledgements

This package would not exist in its current format without the
inspiration of [Bob Rudis’s](https://rud.is/b/) [lodes
package](https://github.com/hrbrmstr/lodes)
