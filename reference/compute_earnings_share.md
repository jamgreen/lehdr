# Compute earnings tier shares from LODES RAC or WAC data

Computes the share of jobs (or workers) in each of the three LODES
monthly earnings tiers:

- Low (`CE01`):

  Earnings up to \$1,250/month.

- Mid (`CE02`):

  Earnings \$1,251-\$3,333/month.

- High (`CE03`):

  Earnings above \$3,333/month.

Earnings shares are useful for tracking wage polarization, identifying
low-wage job concentration, and examining how the earnings structure of
a labor market has shifted over time, especially when combined with
[`compute_lodes_change()`](https://jamgreen.github.io/lehdr/reference/compute_lodes_change.md).

The total denominator is the sum of the three tiers, ensuring shares sum
to 1 within rounding error.

## Usage

``` r
compute_earnings_share(
  lodes_df,
  type = c("wac", "rac"),
  geo_col = NULL,
  output = c("wide", "long")
)
```

## Arguments

- lodes_df:

  A data frame (tibble) of LODES RAC or WAC data returned by
  [`grab_lodes()`](https://jamgreen.github.io/lehdr/reference/grab_lodes.md)
  with `segment = "S000"` (the total count segment, which includes all
  three earnings columns). Must contain columns `CE01`, `CE02`, and
  `CE03`. Both WAC and RAC files include these earnings tier columns;
  the `type` argument controls only which geography prefix is used for
  auto-detection.

- type:

  One of `"wac"` (workplace area characteristics, default) or `"rac"`
  (residential area characteristics). Controls the expected geography
  column prefix (`w_` for WAC, `h_` for RAC) when `geo_col = NULL`. Does
  not affect which earnings columns are used, as both file types share
  the `CE01`/`CE02`/`CE03` schema.

- geo_col:

  The name of the geography column to group by, e.g. `"w_tract"` or
  `"h_county"`. Defaults to `NULL`, which auto-detects the first
  geography column in `lodes_df`.

- output:

  One of `"wide"` (default) or `"long"`. Wide format appends three share
  columns. Long format returns one row per geography-tier combination.

## Value

A tibble with earnings tier counts and shares. In `"wide"` format,
columns are added for `share_low`, `share_mid`, and `share_high`. In
`"long"` format, columns are `tier`, `label`, `count`, and `share`.

## Examples

``` r
if (FALSE) { # \dontrun{
  wac <- grab_lodes(
    state = "md", year = 2019,
    lodes_type = "wac", job_type = "JT00",
    segment = "S000", agg_geo = "county"
  )
  compute_earnings_share(wac, type = "wac", geo_col = "w_county")

  # Long format, suitable for ggplot2
  compute_earnings_share(
    wac, type = "wac", geo_col = "w_county", output = "long"
  )
} # }
```
