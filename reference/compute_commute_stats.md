# Compute commute flow statistics from LODES OD data

Derives three key commute flow metrics from an OD tibble: inflow,
outflow, net flow, and the self-containment ratio. These metrics are
widely used in transportation planning and economic geography to
characterize labor market catchment areas and job/housing balance.

Self-containment is defined as the proportion of workers who both live
and work within the same geographic unit, relative to all employed
residents. A value close to 1 indicates a highly self-contained labor
market; values near 0 indicate heavy out-commuting.

Net flow is defined as inbound workers minus outbound workers (including
internal flows as both in and out). This is an unsigned flow balance
indicator: positive values signal net job importers (more workers arrive
than leave); negative values signal net exporters.

**Cross-state commuters:** When `state_part = "main"` is used in
[`grab_lodes()`](https://dillonma.github.io/lehdr/reference/grab_lodes.md),
only workers who live and work in the same state are included. Workers
who cross state lines (e.g., Maryland residents working in DC) appear
only in `state_part = "aux"` files for the *workplace* state. To capture
full commute flows for border counties, retrieve both `"main"` and
`"aux"` files and bind the rows before calling
`compute_commute_stats()`.

## Usage

``` r
compute_commute_stats(od_df, agg_geo = "tract")
```

## Arguments

- od_df:

  A data frame (tibble) of LODES origin-destination data returned by
  [`grab_lodes()`](https://dillonma.github.io/lehdr/reference/grab_lodes.md)
  with `lodes_type = "od"`. Must include columns `h_{agg_geo}` (home
  geography), `w_{agg_geo}` (work geography), and `S000` (total job
  count). The data frame may be at any aggregation level supported by
  `agg_geo`.

  **Note on row structure:** LODES OD files are a flow matrix. A call to
  [`grab_lodes()`](https://dillonma.github.io/lehdr/reference/grab_lodes.md)
  with `lodes_type = "od"` returns one row per *origin-destination
  pair*, not one row per geography – even after aggregation via
  `agg_geo`. For example, a county-level OD pull for West Virginia
  returns ~2,800 rows (one per observed county-county flow pair), not 55
  (the number of counties). Pass the result directly to
  `compute_commute_stats()` to reduce the pair table to one row per
  geography with inflow, outflow, net flow, and self-containment.

- agg_geo:

  The geographic level of the OD data. Must match the level at which
  `od_df` was retrieved or aggregated. One of `"block"`, `"bg"`,
  `"tract"`, `"county"`, or `"state"`. Defaults to `"tract"`.

## Value

A tibble with one row per geography, containing:

- `{agg_geo}`:

  The geographic identifier.

- `year`:

  Year of the data (if present in `od_df`).

- `state`:

  State FIPS abbreviation (if present in `od_df`).

- `workers_in`:

  Total workers arriving (working in this geography).

- `workers_out`:

  Total workers departing (living here, working elsewhere).

- `workers_internal`:

  Workers whose home and work are both in this geography (internal
  flows).

- `net_flow`:

  Net worker flow: `workers_in - workers_out`. Positive values indicate
  net job importers.

- `self_containment`:

  Share of resident workers who also work in this geography:
  `workers_internal / workers_out_total`, where `workers_out_total`
  includes internal flows.

## Examples

``` r
# \donttest{
  od <- grab_lodes(
    state = "md", year = 2019,
    lodes_type = "od", job_type = "JT00",
    segment = "S000", state_part = "main",
    agg_geo = "county"
  )
#> Downloading https://lehd.ces.census.gov/data/lodes/LODES8/md/od/md_od_main_JT00_2019.csv.gz to lodes8_md_od_main_JT00_2019.csv.gz...
#> Download complete: lodes8_md_od_main_JT00_2019.csv.gz
#> lodes8_md_od_main_JT00_2019.csv.gz cleared from cache.
  compute_commute_stats(od, agg_geo = "county")
#> compute_commute_stats(): reducing 576 origin-destination pairs to one row per county.
#> # A tibble: 24 × 8
#>    county  year state workers_in workers_out workers_internal net_flow
#>    <chr>  <int> <chr>      <dbl>       <dbl>            <dbl>    <dbl>
#>  1 24001   2019 MD         20669        5676            16866    -1873
#>  2 24003   2019 MD        252820      130836           110416    11568
#>  3 24005   2019 MD        368877      223785           169441   -24349
#>  4 24009   2019 MD         20325       19774            12124   -11573
#>  5 24011   2019 MD          7256       11111             3713    -7568
#>  6 24013   2019 MD         49617       54659            26959   -32001
#>  7 24015   2019 MD         22114       15977            15076    -8939
#>  8 24017   2019 MD         32785       32125            16699   -16039
#>  9 24019   2019 MD         10550        9010             5421    -3881
#> 10 24021   2019 MD         88894       63841            50939   -25886
#> # ℹ 14 more rows
#> # ℹ 1 more variable: self_containment <dbl>
# }
```
