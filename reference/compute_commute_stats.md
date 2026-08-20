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
[`grab_lodes()`](https://jamgreen.github.io/lehdr/reference/grab_lodes.md),
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
  [`grab_lodes()`](https://jamgreen.github.io/lehdr/reference/grab_lodes.md)
  with `lodes_type = "od"`. Must include columns `h_{agg_geo}` (home
  geography), `w_{agg_geo}` (work geography), and `S000` (total job
  count). The data frame may be at any aggregation level supported by
  `agg_geo`.

  **Note on row structure:** LODES OD files are a flow matrix. A call to
  [`grab_lodes()`](https://jamgreen.github.io/lehdr/reference/grab_lodes.md)
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
if (FALSE) { # \dontrun{
  od <- grab_lodes(
    state = "md", year = 2019,
    lodes_type = "od", job_type = "JT00",
    segment = "S000", state_part = "main",
    agg_geo = "county"
  )
  compute_commute_stats(od, agg_geo = "county")
} # }
```
