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
  [`grab_lodes()`](https://dillonma.github.io/lehdr/reference/grab_lodes.md)
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
# \donttest{
  wac_multi <- grab_lodes(
    state = "md", year = c(2015, 2019),
    lodes_type = "wac", job_type = "JT00",
    segment = "S000", agg_geo = "county"
  )
#> Downloading https://lehd.ces.census.gov/data/lodes/LODES8/md/wac/md_wac_S000_JT00_2015.csv.gz to lodes8_md_wac_S000_JT00_2015.csv.gz...
#> Download complete: lodes8_md_wac_S000_JT00_2015.csv.gz
#> lodes8_md_wac_S000_JT00_2015.csv.gz cleared from cache.
#> Downloading https://lehd.ces.census.gov/data/lodes/LODES8/md/wac/md_wac_S000_JT00_2019.csv.gz to lodes8_md_wac_S000_JT00_2019.csv.gz...
#> Download complete: lodes8_md_wac_S000_JT00_2019.csv.gz
#> lodes8_md_wac_S000_JT00_2019.csv.gz cleared from cache.
  compute_lodes_change(wac_multi, geo_col = "w_county")
#> # A tibble: 24 × 206
#>    w_county state C000_base CA01_base CA02_base CA03_base CE01_base CE02_base
#>    <chr>    <chr>     <dbl>     <dbl>     <dbl>     <dbl>     <dbl>     <dbl>
#>  1 24001    MD        27705      6134     14795      6776      7596     11227
#>  2 24003    MD       257644     62430    136676     58538     59465     76402
#>  3 24005    MD       376473     89637    197685     89151     92004    117928
#>  4 24009    MD        21709      5542     11292      4875      5819      7084
#>  5 24011    MD         8502      1696      4502      2304      1780      3414
#>  6 24013    MD        57748     14436     28877     14435     16164     19685
#>  7 24015    MD        28963      7271     15186      6506      7061      9939
#>  8 24017    MD        37835     10428     19379      8028     11104     12475
#>  9 24019    MD        10810      2296      5476      3038      2513      4666
#> 10 24021    MD        95932     21874     52087     21971     22770     31070
#> # ℹ 14 more rows
#> # ℹ 198 more variables: CE03_base <dbl>, CNS01_base <dbl>, CNS02_base <dbl>,
#> #   CNS03_base <dbl>, CNS04_base <dbl>, CNS05_base <dbl>, CNS06_base <dbl>,
#> #   CNS07_base <dbl>, CNS08_base <dbl>, CNS09_base <dbl>, CNS10_base <dbl>,
#> #   CNS11_base <dbl>, CNS12_base <dbl>, CNS13_base <dbl>, CNS14_base <dbl>,
#> #   CNS15_base <dbl>, CNS16_base <dbl>, CNS17_base <dbl>, CNS18_base <dbl>,
#> #   CNS19_base <dbl>, CNS20_base <dbl>, CR01_base <dbl>, CR02_base <dbl>, …

  # Long format
  compute_lodes_change(
    wac_multi,
    geo_col  = "w_county",
    output   = "long",
    variables = c("C000", "CE01", "CE02", "CE03")
  )
#> # A tibble: 96 × 8
#>    w_county variable base_year compare_year base_value compare_value change
#>    <chr>    <chr>        <int>        <int>      <dbl>         <dbl>  <dbl>
#>  1 24001    C000          2015         2019      27705         27898    193
#>  2 24003    C000          2015         2019     257644        270513  12869
#>  3 24005    C000          2015         2019     376473        398040  21567
#>  4 24009    C000          2015         2019      21709         21637    -72
#>  5 24011    C000          2015         2019       8502          9102    600
#>  6 24013    C000          2015         2019      57748         57878    130
#>  7 24015    C000          2015         2019      28963         31140   2177
#>  8 24017    C000          2015         2019      37835         37316   -519
#>  9 24019    C000          2015         2019      10810         11902   1092
#> 10 24021    C000          2015         2019      95932        104114   8182
#> # ℹ 86 more rows
#> # ℹ 1 more variable: pct_change <dbl>
# }
```
