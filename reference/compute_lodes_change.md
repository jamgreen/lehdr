# Compute longitudinal change in LODES data across years

Computes absolute and percentage change in LODES variables between two
years for each geographic unit. This is particularly useful for tracking
shifts in employment, industrial composition, or earnings structure over
time.

Percentage change is computed as \\(\text{compare} - \text{base}) /
\text{base} \times 100\\. Returns `NA` where `base_value` is zero or
missing.

## Usage

``` r
compute_lodes_change(
  lodes_df,
  geo_col = NULL,
  base_year = NULL,
  compare_year = NULL,
  variables = NULL,
  output = c("wide", "long")
)
```

## Arguments

- lodes_df:

  A data frame (tibble) of LODES data covering at least two years, as
  returned by
  [`grab_lodes()`](https://jamgreen.github.io/lehdr/reference/grab_lodes.md)
  with a vector of years. Must contain a `year` column and one or more
  numeric columns to difference.

- geo_col:

  The name of the geography column to group by, e.g. `"w_tract"`,
  `"h_county"`, or `"w_geocode"`. Defaults to `NULL`, which auto-detects
  the first column ending in `_geocode`, `_tract`, `_county`, `_bg`, or
  `_state`.

- base_year:

  The reference year for computing change. Defaults to the earliest year
  present in `lodes_df`.

- compare_year:

  The target year for computing change. Defaults to the latest year
  present in `lodes_df`.

- variables:

  Optional character vector of numeric column names to include in the
  output. Defaults to all numeric columns (excluding `year`).

- output:

  One of `"wide"` (default) or `"long"`. In wide format, absolute and
  percentage change columns are appended for each variable. In long
  format, each variable is a row with columns `variable`, `base_value`,
  `compare_value`, `change`, and `pct_change`.

## Value

A tibble of change statistics. In `"wide"` format, columns follow the
pattern `{variable}_base`, `{variable}_compare`, `{variable}_change`,
and `{variable}_pct_change`. In `"long"` format, columns are `variable`,
`base_value`, `compare_value`, `change`, and `pct_change`.

## Examples

``` r
if (FALSE) { # \dontrun{
  wac_multi <- grab_lodes(
    state = "md", year = c(2015, 2019),
    lodes_type = "wac", job_type = "JT00",
    segment = "S000", agg_geo = "county"
  )
  compute_lodes_change(wac_multi, geo_col = "w_county")

  # Long format
  compute_lodes_change(
    wac_multi,
    geo_col  = "w_county",
    output   = "long",
    variables = c("C000", "CE01", "CE02", "CE03")
  )
} # }
```
