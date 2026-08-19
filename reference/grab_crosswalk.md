# Download and load LODES geographic crosswalk into a data frame (tibble)

Download the LODES geographic crosswalk for one or more states. The
crosswalk maps Census block GEOIDs to higher-level geographies and is
useful for custom aggregations outside the built-in `agg_geo` argument
in
[`grab_lodes()`](https://dillonma.github.io/lehdr/reference/grab_lodes.md).

## Usage

``` r
grab_crosswalk(
  state,
  version = c("LODES8", "LODES7", "LODES5"),
  download_dir = normalizePath(file.path(tools::R_user_dir("lehdr", which = "cache")),
    mustWork = FALSE)
)
```

## Arguments

- state:

  US state abbreviation in lower case. Can be a vector of state
  abbreviations, e.g. `c("wy", "nd", "sd")`.

- version:

  The LODES version whose crosswalk to download. Must be one of
  `"LODES8"` (default), `"LODES7"`, or `"LODES5"`. The crosswalk maps
  Census blocks to higher-level geographies for that vintage.

- download_dir:

  Directory where the crosswalk file will be downloaded. Defaults to the
  user-level cache directory for `lehdr`.

## Value

A tibble containing the geographic crosswalk at the Census block level,
with columns linking blocks to block groups, tracts, counties, and
states.

## Examples

``` r
# \donttest{
  # Download crosswalk for Vermont
  vt_xwalk <- grab_crosswalk("vt")
#> Downloading crosswalk https://lehd.ces.census.gov/data/lodes/LODES8/vt/vt_xwalk.csv.gz to lodes8_vt_xwalk.csv.gz
#> Download complete for lodes8_vt_xwalk.csv.gz
#> lodes8_vt_xwalk.csv.gz crosswalk cleared from cache.

  # Download crosswalk for several small states
  small_xwalk <- grab_crosswalk(c("wy", "nd", "sd"))
#> Downloading crosswalk https://lehd.ces.census.gov/data/lodes/LODES8/wy/wy_xwalk.csv.gz to lodes8_wy_xwalk.csv.gz
#> Download complete for lodes8_wy_xwalk.csv.gz
#> Downloading crosswalk https://lehd.ces.census.gov/data/lodes/LODES8/nd/nd_xwalk.csv.gz to lodes8_nd_xwalk.csv.gz
#> Download complete for lodes8_nd_xwalk.csv.gz
#> Downloading crosswalk https://lehd.ces.census.gov/data/lodes/LODES8/sd/sd_xwalk.csv.gz to lodes8_sd_xwalk.csv.gz
#> Download complete for lodes8_sd_xwalk.csv.gz
#> lodes8_wy_xwalk.csv.gz crosswalk cleared from cache.
#> lodes8_nd_xwalk.csv.gz crosswalk cleared from cache.
#> lodes8_sd_xwalk.csv.gz crosswalk cleared from cache.

  # Download a LODES7 crosswalk (2010 Census block vintage)
  vt_xwalk_7 <- grab_crosswalk("vt", version = "LODES7")
#> Downloading crosswalk https://lehd.ces.census.gov/data/lodes/LODES7/vt/vt_xwalk.csv.gz to lodes7_vt_xwalk.csv.gz
#> Download complete for lodes7_vt_xwalk.csv.gz
#> lodes7_vt_xwalk.csv.gz crosswalk cleared from cache.
# }
```
