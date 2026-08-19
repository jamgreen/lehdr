# Download and load LODES data into a data frame (tibble)

Download LODES OD, RAC, and WAC tables from the LEHD FTP server and
return a tidy data frame.

## Usage

``` r
grab_lodes(
  state,
  year,
  version = c("LODES8", "LODES7", "LODES5"),
  lodes_type = c("od", "rac", "wac"),
  job_type = c("JT00", "JT01", "JT02", "JT03", "JT04", "JT05"),
  segment = c("S000", "SA01", "SA02", "SA03", "SE01", "SE02", "SE03", "SI01", "SI02",
    "SI03"),
  agg_geo = c("block", "bg", "tract", "county", "state"),
  state_part = c("", "main", "aux"),
  download_dir = normalizePath(file.path(tools::R_user_dir("lehdr", which = "cache")),
    mustWork = FALSE),
  geometry = FALSE,
  use_cache = getOption("lehdr_use_cache", FALSE),
  ...
)
```

## Arguments

- state:

  US state abbreviation in lower case, as character. Can be a vector of
  states, like `c("or", "md", "tx")` for Oregon, Maryland, and Texas.
  Two-letter FIPS abbreviations only.

- year:

  Year of the LODES data, as a numeric integer. Can be a vector of
  years, like `c(2014, 2020)` for 2014 and 2020. Must be between 2002
  and 2023.

- version:

  The LODES version to use. `"LODES8"` (the default) is enumerated at
  2020 Census blocks and covers 2002-2022. `"LODES7"` is enumerated at
  2010 Census blocks and covers 2002-2019. `"LODES5"` is enumerated at
  2000 Census blocks and covers 2002-2009.

- lodes_type:

  The LODES table type. Values can be the default origin-destination
  (`"od"`), residential area characteristics (`"rac"`), or workplace
  area characteristics (`"wac"`). OD files give a home and workplace
  census block for each worker flow. RAC files give job totals at worker
  home blocks; WAC files give job totals at worker job blocks.

- job_type:

  Job type segment: `"JT00"` for all jobs (default), `"JT01"` for
  Primary Jobs, `"JT02"` for All Private Jobs, `"JT03"` for Private
  Primary jobs, `"JT04"` for All Federal jobs, `"JT05"` for Federal
  Primary jobs.

- segment:

  Workforce segment. `"S000"` total jobs (default); `"SA01"` workers
  aged 29 or younger; `"SA02"` workers aged 30-54; `"SA03"` workers 55
  and older; `"SE01"` earnings \$1,250/month or less; `"SE02"` earnings
  \$1,251-\$3,333/month; `"SE03"` earnings above \$3,333/month; `"SI01"`
  Goods Producing industries; `"SI02"` Trade, Transportation, &
  Utilities industries; `"SI03"` All Other Services.

- agg_geo:

  Aggregate to a geography other than Census Block (default). Values can
  be `"bg"` (block group), `"tract"`, `"county"`, or `"state"`. The
  string `"block group"` is also accepted as an alias for `"bg"`.

- state_part:

  Part of the state file; required when `lodes_type = "od"`. `"main"`
  includes workers whose home and workplace are both in-state. `"aux"`
  includes workers who live out-of-state but work in the state of
  interest. Defaults to `"main"` with a warning when `lodes_type = "od"`
  and `state_part` is not explicitly supplied.

- download_dir:

  Directory where the LODES file will be downloaded. Defaults to the
  user-level cache directory for `lehdr`.

- geometry:

  If `TRUE`, use the `tigris` package to download and attach spatial
  geometries from the U.S. Census Bureau for the specified `year` at the
  level specified by `agg_geo`. Returns an `sf` object when `lodes_type`
  is `"rac"` or `"wac"`.

- use_cache:

  Boolean. If `TRUE`, reuse previously downloaded files rather than
  re-downloading. Defaults to `FALSE`. You can also set the
  `lehdr_use_cache` global option to `TRUE` to make caching the default
  for a session.

- ...:

  Additional arguments passed to `tigris` functions when
  `geometry = TRUE`.

## Value

A tibble of LODES data aggregated to block, block group, tract, county,
or state level. If `geometry = TRUE` and `lodes_type` is `"rac"` or
`"wac"`, returns an `sf` object.

## Examples

``` r
# \donttest{
  # Download 2014 block-level OD data for Vermont
  blk_od <- grab_lodes(
    state = "vt", year = 2014,
    lodes_type = "od", job_type = "JT01",
    segment = "SA01", state_part = "main"
  )
#> Downloading https://lehd.ces.census.gov/data/lodes/LODES8/vt/od/vt_od_main_JT01_2014.csv.gz to lodes8_vt_od_main_JT01_2014.csv.gz...
#> Download complete: lodes8_vt_od_main_JT01_2014.csv.gz
#> lodes8_vt_od_main_JT01_2014.csv.gz cleared from cache.

  # Download 2014 OD data for Vermont aggregated to tract level
  trt_od <- grab_lodes(
    state = "vt", year = 2014,
    lodes_type = "od", job_type = "JT01",
    segment = "SA01", state_part = "main",
    agg_geo = "tract"
  )
#> Downloading https://lehd.ces.census.gov/data/lodes/LODES8/vt/od/vt_od_main_JT01_2014.csv.gz to lodes8_vt_od_main_JT01_2014.csv.gz...
#> Download complete: lodes8_vt_od_main_JT01_2014.csv.gz
#> lodes8_vt_od_main_JT01_2014.csv.gz cleared from cache.

  # Download 2020 RAC data for Vermont aggregated to tract level
  trt_rac <- grab_lodes(
    state = "vt", year = 2020,
    lodes_type = "rac", job_type = "JT01",
    segment = "SA01", agg_geo = "tract"
  )
#> Downloading https://lehd.ces.census.gov/data/lodes/LODES8/vt/rac/vt_rac_SA01_JT01_2020.csv.gz to lodes8_vt_rac_SA01_JT01_2020.csv.gz...
#> Download complete: lodes8_vt_rac_SA01_JT01_2020.csv.gz
#> lodes8_vt_rac_SA01_JT01_2020.csv.gz cleared from cache.

  # Download 2020 WAC data for Vermont aggregated to tract level
  trt_wac <- grab_lodes(
    state = "vt", year = 2020,
    lodes_type = "wac", job_type = "JT01",
    segment = "SA01", agg_geo = "tract"
  )
#> Downloading https://lehd.ces.census.gov/data/lodes/LODES8/vt/wac/vt_wac_SA01_JT01_2020.csv.gz to lodes8_vt_wac_SA01_JT01_2020.csv.gz...
#> Download complete: lodes8_vt_wac_SA01_JT01_2020.csv.gz
#> lodes8_vt_wac_SA01_JT01_2020.csv.gz cleared from cache.
# }
```
