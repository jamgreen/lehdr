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
[`compute_lodes_change()`](https://dillonma.github.io/lehdr/reference/compute_lodes_change.md).

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
  [`grab_lodes()`](https://dillonma.github.io/lehdr/reference/grab_lodes.md)
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
# \donttest{
  wac <- grab_lodes(
    state = "md", year = 2019,
    lodes_type = "wac", job_type = "JT00",
    segment = "S000", agg_geo = "county"
  )
#> Downloading https://lehd.ces.census.gov/data/lodes/LODES8/md/wac/md_wac_S000_JT00_2019.csv.gz to lodes8_md_wac_S000_JT00_2019.csv.gz...
#> Download complete: lodes8_md_wac_S000_JT00_2019.csv.gz
#> lodes8_md_wac_S000_JT00_2019.csv.gz cleared from cache.
  compute_earnings_share(wac, type = "wac", geo_col = "w_county")
#> # A tibble: 24 × 10
#>    w_county  year state count_low count_mid count_high total_earnings share_low
#>    <chr>    <int> <chr>     <dbl>     <dbl>      <dbl>          <dbl>     <dbl>
#>  1 24001     2019 MD         6703     10657      10538          27898     0.240
#>  2 24003     2019 MD        56151     75065     139297         270513     0.208
#>  3 24005     2019 MD        85069    112452     200519         398040     0.214
#>  4 24009     2019 MD         5596      6699       9342          21637     0.259
#>  5 24011     2019 MD         1794      3460       3848           9102     0.197
#>  6 24013     2019 MD        14972     19219      23687          57878     0.259
#>  7 24015     2019 MD         6624      9991      14525          31140     0.213
#>  8 24017     2019 MD        10043     12431      14842          37316     0.269
#>  9 24019     2019 MD         2422      4597       4883          11902     0.203
#> 10 24021     2019 MD        22563     31364      50187         104114     0.217
#> # ℹ 14 more rows
#> # ℹ 2 more variables: share_mid <dbl>, share_high <dbl>

  # Long format, suitable for ggplot2
  compute_earnings_share(
    wac, type = "wac", geo_col = "w_county", output = "long"
  )
#> # A tibble: 72 × 7
#>    w_county  year state tier  label                    count share
#>    <chr>    <int> <chr> <chr> <chr>                    <dbl> <dbl>
#>  1 24001     2019 MD    low   CE01: up to $1,250/month  6703 0.240
#>  2 24003     2019 MD    low   CE01: up to $1,250/month 56151 0.208
#>  3 24005     2019 MD    low   CE01: up to $1,250/month 85069 0.214
#>  4 24009     2019 MD    low   CE01: up to $1,250/month  5596 0.259
#>  5 24011     2019 MD    low   CE01: up to $1,250/month  1794 0.197
#>  6 24013     2019 MD    low   CE01: up to $1,250/month 14972 0.259
#>  7 24015     2019 MD    low   CE01: up to $1,250/month  6624 0.213
#>  8 24017     2019 MD    low   CE01: up to $1,250/month 10043 0.269
#>  9 24019     2019 MD    low   CE01: up to $1,250/month  2422 0.203
#> 10 24021     2019 MD    low   CE01: up to $1,250/month 22563 0.217
#> # ℹ 62 more rows
# }
```
