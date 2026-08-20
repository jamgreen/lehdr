# Download and load LODES geographic crosswalk into a data frame (tibble)

Download the LODES geographic crosswalk for one or more states. The
crosswalk maps Census block GEOIDs to higher-level geographies and is
useful for custom aggregations outside the built-in `agg_geo` argument
in
[`grab_lodes()`](https://jamgreen.github.io/lehdr/reference/grab_lodes.md).

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
if (FALSE) { # \dontrun{
  # Download crosswalk for Vermont
  vt_xwalk <- grab_crosswalk("vt")

  # Download crosswalk for several small states
  small_xwalk <- grab_crosswalk(c("wy", "nd", "sd"))

  # Download a LODES7 crosswalk (2010 Census block vintage)
  vt_xwalk_7 <- grab_crosswalk("vt", version = "LODES7")
} # }
```
