# ============================================================
# Tests for the base fucntionality of the lehdr package
#
# Test states: DE (Delaware), VT (Vermont), ND (North Dakota),
# SD (South Dakota), WY (Wyoming). These states are used because
# their LODES files are relatively small and fast to download.
#
# All tests use withr::local_options(list(lehdr_use_cache = TRUE))
# to avoid redundant downloads within a test run.
# ============================================================

# ---- grab_lodes(): single state, OD ---------------------------

test_that("grab_lodes returns correct dimensions for OD tract-level LODES8", {
  withr::local_options(list(lehdr_use_cache = TRUE))

  expect_equal(
    grab_lodes(
      state      = "de",
      year       = 2020,
      version    = "LODES8",
      lodes_type = "od",
      job_type   = "JT00",
      segment    = "SA01",
      state_part = "main",
      agg_geo    = "tract"
    ) %>% dim(),
    c(36671, 14)
  )
})

test_that("grab_lodes returns correct dimensions for OD tract-level LODES5", {
  withr::local_options(list(lehdr_use_cache = TRUE))

  expect_equal(
    grab_lodes(
      state      = "de",
      year       = 2009,
      version    = "LODES5",
      lodes_type = "od",
      state_part = "main",
      agg_geo    = "tract"
    ) %>% dim(),
    c(25805, 14)
  )
})

test_that("grab_lodes accepts year as character string for OD block-level LODES8", {
  withr::local_options(list(lehdr_use_cache = TRUE))

  # year = "2015" (character) should coerce silently
  expect_equal(
    grab_lodes(
      state      = "de",
      year       = "2015",
      version    = "LODES8",
      lodes_type = "od",
      job_type   = "JT00",
      segment    = "SE01",
      state_part = "main"
    ) %>% dim(),
    c(290314, 15)
  )
})

test_that("grab_lodes returns correct dimensions for OD aux state_part LODES5", {
  withr::local_options(list(lehdr_use_cache = TRUE))

  expect_equal(
    grab_lodes(
      state      = "de",
      year       = "2009",
      version    = "LODES5",
      lodes_type = "od",
      state_part = "aux"
    ) %>% dim(),
    c(68588, 15)
  )
})

test_that("grab_lodes warns and defaults state_part to main when omitted for OD", {
  withr::local_options(list(lehdr_use_cache = TRUE))

  expect_warning(
    grab_lodes(
      state      = "de",
      year       = 2020,
      version    = "LODES8",
      lodes_type = "od",
      job_type   = "JT00",
      segment    = "SA01"
    ),
    "state_part"
  )
})

# ---- grab_lodes(): single state, RAC --------------------------

test_that("grab_lodes returns correct dimensions for RAC tract-level LODES7", {
  withr::local_options(list(lehdr_use_cache = TRUE))

  expect_equal(
    grab_lodes(
      state      = "de",
      year       = 2014,
      version    = "LODES7",
      lodes_type = "rac",
      agg_geo    = "tract"
    ) %>% dim(),
    c(218, 44)
  )
})

test_that("grab_lodes returns correct dimensions for RAC block-level LODES8", {
  withr::local_options(list(lehdr_use_cache = TRUE))

  expect_equal(
    grab_lodes(
      state      = "de",
      year       = "2015",
      version    = "LODES8",
      lodes_type = "rac"
    ) %>% dim(),
    c(14436, 45)
  )
})

test_that("grab_lodes returns correct dimensions for RAC with non-default job_type and segment", {
  withr::local_options(list(lehdr_use_cache = TRUE))

  expect_equal(
    grab_lodes(
      state      = "de",
      year       = "2004",
      version    = "LODES7",
      lodes_type = "rac",
      job_type   = "JT01",
      segment    = "SA01"
    ) %>% dim(),
    c(12596, 45)
  )
})

# ---- grab_lodes(): single state, WAC --------------------------

test_that("grab_lodes returns correct dimensions for WAC tract-level LODES5", {
  withr::local_options(list(lehdr_use_cache = TRUE))

  expect_equal(
    grab_lodes(
      state      = "de",
      year       = 2009,
      version    = "LODES5",
      lodes_type = "wac",
      job_type   = "JT01",
      agg_geo    = "tract"
    ) %>% dim(),
    c(197, 42)
  )
})

test_that("grab_lodes returns correct dimensions for WAC block-level LODES7", {
  withr::local_options(list(lehdr_use_cache = TRUE))

  expect_equal(
    grab_lodes(
      state      = "de",
      year       = "2015",
      version    = "LODES7",
      lodes_type = "wac"
    ) %>% dim(),
    c(5476, 55)
  )
})

test_that("grab_lodes returns correct dimensions for WAC block-level LODES8", {
  withr::local_options(list(lehdr_use_cache = TRUE))

  expect_equal(
    grab_lodes(
      state      = "de",
      year       = "2020",
      version    = "LODES8",
      lodes_type = "wac"
    ) %>% dim(),
    c(6421, 55)
  )
})

# LODES7 and LODES8 use different Census block vintages and cover different
# year ranges, so results for the same state differ in row count. Block-level
# column schema is identical across versions (both WAC files have 55 columns
# plus year and state), so this test documents the row-count difference.
test_that("grab_lodes LODES7 and LODES8 WAC differ in row count for the same year", {
  withr::local_options(list(lehdr_use_cache = TRUE))

  lodes7_rows <- grab_lodes(
    state      = "vt",
    year       = 2015,
    version    = "LODES7",
    lodes_type = "wac"
  ) %>% nrow()

  lodes8_rows <- grab_lodes(
    state      = "vt",
    year       = 2015,
    version    = "LODES8",
    lodes_type = "wac"
  ) %>% nrow()

  # Different block vintages enumerate different numbers of blocks
  # Note this test does not work for DE since the count of blocks remains
  # the same across the vintages
  expect_false(lodes7_rows == lodes8_rows)
})

# ---- grab_lodes(): input validation ---------------------------

test_that("grab_lodes aborts on unrecognized state abbreviation", {
  expect_error(
    grab_lodes(
      state      = "zz",
      year       = 2020,
      version    = "LODES8",
      lodes_type = "wac"
    ),
    "not a recognized"
  )
})

test_that("grab_lodes aborts on mixed-case invalid state", {
  expect_error(
    grab_lodes(
      state      = "MARYLAND",
      year       = 2020,
      version    = "LODES8",
      lodes_type = "wac"
    ),
    "not a recognized"
  )
})

test_that("grab_lodes aborts when year is out of range for LODES8", {
  expect_error(
    grab_lodes(
      state      = "de",
      year       = 2023,
      version    = "LODES8",
      lodes_type = "wac"
    ),
    "outside the valid range"
  )
})

test_that("grab_lodes aborts when year is out of range for LODES5", {
  expect_error(
    grab_lodes(
      state      = "de",
      year       = 2015,
      version    = "LODES5",
      lodes_type = "wac"
    ),
    "outside the valid range"
  )
})

test_that("grab_lodes aborts when year is out of range for LODES7", {
  expect_error(
    grab_lodes(
      state      = "de",
      year       = 2020,
      version    = "LODES7",
      lodes_type = "wac"
    ),
    "outside the valid range"
  )
})

test_that("grab_lodes aborts on non-logical use_cache", {
  expect_error(
    grab_lodes(
      state      = "de",
      year       = 2020,
      version    = "LODES8",
      lodes_type = "wac",
      use_cache  = "yes"
    ),
    "use_cache"
  )
})

# ---- grab_lodes(): multiple states ----------------------------

test_that("grab_lodes returns correct dimensions for multi-state OD LODES5", {
  withr::local_options(list(lehdr_use_cache = TRUE))

  expect_equal(
    grab_lodes(
      state      = c("nd", "vt"),
      year       = c(2007, 2008),
      version    = "LODES5",
      lodes_type = "od",
      job_type   = "JT01",
      segment    = "SA01",
      state_part = "main",
      agg_geo    = "tract"
    ) %>% dim(),
    c(65262, 14)
  )
})

test_that("grab_lodes returns correct dimensions for multi-state OD LODES8", {
  withr::local_options(list(lehdr_use_cache = TRUE))

  expect_equal(
    grab_lodes(
      state      = c("de", "vt"),
      year       = c(2013, 2014),
      version    = "LODES8",
      lodes_type = "od",
      job_type   = "JT01",
      segment    = "SA01",
      state_part = "main"
    ) %>% dim(),
    c(929717, 15)
  )
})

test_that("grab_lodes returns correct dimensions for cross-year multi-state OD LODES8", {
  withr::local_options(list(lehdr_use_cache = TRUE))

  expect_equal(
    grab_lodes(
      state      = c("de", "sd"),
      year       = c(2013, 2020),
      version    = "LODES8",
      lodes_type = "od",
      job_type   = "JT01",
      segment    = "SA01",
      state_part = "main"
    ) %>% dim(),
    c(1125184, 15)
  )
})

# ---- grab_lodes(): multiple states and years, WAC -------------

test_that("grab_lodes returns correct dimensions for multi-state WAC tract-level LODES7", {
  withr::local_options(list(lehdr_use_cache = TRUE))

  expect_equal(
    grab_lodes(
      state      = c("de", "vt"),
      year       = c(2013, 2014),
      version    = "LODES7",
      lodes_type = "wac",
      agg_geo    = "tract"
    ) %>% dim(),
    c(798, 54)
  )
})

test_that("grab_lodes returns correct dimensions for multi-state WAC tract-level LODES5", {
  withr::local_options(list(lehdr_use_cache = TRUE))

  expect_equal(
    grab_lodes(
      state      = c("de", "vt"),
      year       = c(2007, 2009),
      version    = "LODES5",
      lodes_type = "wac",
      agg_geo    = "tract"
    ) %>% dim(),
    c(752, 42)
  )
})

test_that("grab_lodes returns correct dimensions for multi-state WAC block-level LODES7", {
  withr::local_options(list(lehdr_use_cache = TRUE))

  expect_equal(
    grab_lodes(
      state      = c("de", "vt"),
      year       = c(2013, 2014),
      version    = "LODES7",
      lodes_type = "wac"
    ) %>% dim(),
    c(24132, 55)
  )
})

test_that("grab_lodes returns correct dimensions for multi-year WAC block-level LODES8", {
  withr::local_options(list(lehdr_use_cache = TRUE))

  expect_equal(
    grab_lodes(
      state      = c("de", "vt"),
      year       = c(2017, 2018, 2019, 2020),
      version    = "LODES8",
      lodes_type = "wac",
      job_type   = "JT01",
      segment    = "S000"
    ) %>% dim(),
    c(55696, 55)
  )
})

# The year and state columns must be present and correctly populated
# after the multi-state dispatch path.
test_that("grab_lodes multi-state result contains year and state columns", {
  withr::local_options(list(lehdr_use_cache = TRUE))

  result <- grab_lodes(
    state      = c("de", "vt"),
    year       = c(2019, 2020),
    version    = "LODES8",
    lodes_type = "wac",
    job_type   = "JT00",
    segment    = "S000",
    agg_geo    = "county"
  )

  expect_true("year"  %in% names(result))
  expect_true("state" %in% names(result))
  expect_setequal(unique(result$year),  c(2019L, 2020L))
  expect_setequal(unique(result$state), c("DE", "VT"))
})

# ---- grab_crosswalk() -----------------------------------------

test_that("grab_crosswalk returns correct dimensions for single state", {
  withr::local_options(list(lehdr_use_cache = TRUE))

  expect_equal(
    grab_crosswalk("vt") %>% dim(),
    c(24611, 41)
  )
})

test_that("grab_crosswalk returns correct dimensions for multiple states", {
  withr::local_options(list(lehdr_use_cache = TRUE))

  expect_equal(
    grab_crosswalk(c("wy", "ND")) %>% dim(),
    c(138335, 41)
  )
})

test_that("grab_crosswalk accepts version argument for LODES7", {
  withr::local_options(list(lehdr_use_cache = TRUE))

  result <- grab_crosswalk("vt", version = "LODES7")
  expect_s3_class(result, "tbl_df")
  # LODES7 uses 2010 Census blocks; row count differs from LODES8
  expect_gt(nrow(result), 0L)
})

test_that("grab_crosswalk LODES7 and LODES8 crosswalks differ in row count", {
  withr::local_options(list(lehdr_use_cache = TRUE))

  rows_lodes8 <- nrow(grab_crosswalk("vt", version = "LODES8"))
  rows_lodes7 <- nrow(grab_crosswalk("vt", version = "LODES7"))

  expect_false(rows_lodes8 == rows_lodes7)
})

# ---- join_lodes_geometry() ------------------------------------

test_that("grab_lodes with geometry = TRUE returns sf for RAC county-level LODES5", {
  withr::local_options(list(lehdr_use_cache = TRUE))

  result <- grab_lodes(
    state      = "vt",
    year       = 2008,
    version    = "LODES5",
    lodes_type = "rac",
    job_type   = "JT01",
    segment    = "SA01",
    state_part = "main",
    agg_geo    = "county",
    geometry   = TRUE
  )

  expect_s3_class(result, "sf")
})

test_that("grab_lodes with geometry = TRUE returns sf for WAC county-level LODES5", {
  withr::local_options(list(lehdr_use_cache = TRUE))

  result <- grab_lodes(
    state      = "vt",
    year       = 2008,
    version    = "LODES5",
    lodes_type = "wac",
    job_type   = "JT01",
    segment    = "SA01",
    state_part = "main",
    agg_geo    = "county",
    geometry   = TRUE
  )

  expect_s3_class(result, "sf")
})

test_that("grab_lodes with geometry = TRUE returns sf for RAC block-group LODES8", {
  withr::local_options(list(lehdr_use_cache = TRUE))

  result <- grab_lodes(
    state      = "vt",
    year       = 2022,
    version    = "LODES8",
    lodes_type = "rac",
    job_type   = "JT01",
    segment    = "SA01",
    state_part = "main",
    agg_geo    = "bg",
    geometry   = TRUE
  )

  expect_s3_class(result, "sf")
})

test_that("grab_lodes with geometry = TRUE returns sf for WAC block-group LODES8", {
  withr::local_options(list(lehdr_use_cache = TRUE))

  result <- grab_lodes(
    state      = "vt",
    year       = 2022,
    version    = "LODES8",
    lodes_type = "wac",
    job_type   = "JT01",
    segment    = "SA01",
    state_part = "main",
    agg_geo    = "bg",
    geometry   = TRUE
  )

  expect_s3_class(result, "sf")
})

test_that("grab_lodes with geometry = TRUE returns sf for RAC block-level LODES5", {
  withr::local_options(list(lehdr_use_cache = TRUE))

  result <- grab_lodes(
    state      = "vt",
    year       = 2008,
    version    = "LODES5",
    lodes_type = "rac",
    job_type   = "JT01",
    segment    = "SA01",
    state_part = "main",
    agg_geo    = "block",
    geometry   = TRUE
  )

  expect_s3_class(result, "sf")
})

test_that("grab_lodes with geometry = TRUE returns sf for WAC block-level LODES5", {
  withr::local_options(list(lehdr_use_cache = TRUE))

  result <- grab_lodes(
    state      = "vt",
    year       = 2008,
    version    = "LODES5",
    lodes_type = "wac",
    job_type   = "JT01",
    segment    = "SA01",
    state_part = "main",
    agg_geo    = "block",
    geometry   = TRUE
  )

  expect_s3_class(result, "sf")
})

test_that("grab_lodes with geometry = TRUE and lodes_type = od emits a message", {
  withr::local_options(list(lehdr_use_cache = TRUE))

  expect_message(
    grab_lodes(
      state      = "vt",
      year       = 2008,
      version    = "LODES5",
      lodes_type = "wac",
      job_type   = "JT01",
      segment    = "SA01",
      state_part = "main",
      agg_geo    = "county",
      geometry   = TRUE
    )
  )
})

# ============================================================
# Tests for analytical functions introduced in lehdr v1.2.0
# These tests use DE (Delaware) as the test state because it
# is small and fast to download.
# ============================================================

# ---- compute_commute_stats() ----------------------------------

test_that("compute_commute_stats returns expected columns for tract-level OD", {
  withr::local_options(list(lehdr_use_cache = TRUE))

  od <- grab_lodes(
    state      = "de",
    year       = 2020,
    version    = "LODES8",
    lodes_type = "od",
    job_type   = "JT00",
    segment    = "SA01",
    state_part = "main",
    agg_geo    = "tract"
  )

  result <- compute_commute_stats(od, agg_geo = "tract")

  expect_s3_class(result, "tbl_df")
  expect_named(
    result,
    c("tract", "year", "state",
      "workers_in", "workers_out", "workers_internal",
      "net_flow", "self_containment"),
    ignore.order = TRUE
  )
})

test_that("compute_commute_stats count columns are non-negative", {
  withr::local_options(list(lehdr_use_cache = TRUE))

  od <- grab_lodes(
    state      = "de",
    year       = 2020,
    version    = "LODES8",
    lodes_type = "od",
    job_type   = "JT00",
    segment    = "SA01",
    state_part = "main",
    agg_geo    = "tract"
  )

  result <- compute_commute_stats(od, agg_geo = "tract")

  expect_true(all(result$workers_in       >= 0, na.rm = TRUE))
  expect_true(all(result$workers_out      >= 0, na.rm = TRUE))
  expect_true(all(result$workers_internal >= 0, na.rm = TRUE))
})

test_that("compute_commute_stats self_containment is in [0, 1]", {
  withr::local_options(list(lehdr_use_cache = TRUE))

  od <- grab_lodes(
    state      = "de",
    year       = 2020,
    version    = "LODES8",
    lodes_type = "od",
    job_type   = "JT00",
    segment    = "SA01",
    state_part = "main",
    agg_geo    = "tract"
  )

  result <- compute_commute_stats(od, agg_geo = "tract")
  sc <- result$self_containment[!is.na(result$self_containment)]

  expect_true(all(sc >= 0 & sc <= 1))
})

test_that("compute_commute_stats net_flow equals workers_in minus workers_out_total", {
  withr::local_options(list(lehdr_use_cache = TRUE))

  od <- grab_lodes(
    state      = "de",
    year       = 2020,
    version    = "LODES8",
    lodes_type = "od",
    job_type   = "JT00",
    segment    = "SA01",
    state_part = "main",
    agg_geo    = "tract"
  )

  result <- compute_commute_stats(od, agg_geo = "tract")

  # workers_out_total = workers_internal + workers_out
  workers_out_total <- result$workers_internal + result$workers_out
  expect_equal(result$net_flow, result$workers_in - workers_out_total)
})

test_that("compute_commute_stats works at county level and returns one row per county", {
  withr::local_options(list(lehdr_use_cache = TRUE))

  od <- grab_lodes(
    state      = "de",
    year       = 2020,
    version    = "LODES8",
    lodes_type = "od",
    job_type   = "JT00",
    segment    = "SA01",
    state_part = "main",
    agg_geo    = "county"
  )

  result <- compute_commute_stats(od, agg_geo = "county")

  expect_s3_class(result, "tbl_df")
  expect_true("county" %in% names(result))
  # Delaware has 3 counties
  expect_equal(nrow(result), 3L)
})

test_that("compute_commute_stats aborts when S000 column is missing", {
  bad_df <- data.frame(h_tract = "11001", w_tract = "11002", year = 2020L)

  expect_error(
    compute_commute_stats(bad_df, agg_geo = "tract"),
    "S000"
  )
})

test_that("compute_commute_stats aborts on invalid agg_geo value", {
  od_stub <- data.frame(
    h_tract = character(),
    w_tract = character(),
    S000    = integer(),
    year    = integer()
  )

  expect_error(
    compute_commute_stats(od_stub, agg_geo = "zipcode"),
    "'arg' should be one of"
  )
})

# ---- compute_lodes_change() -----------------------------------

test_that("compute_lodes_change wide output has expected column suffixes", {
  withr::local_options(list(lehdr_use_cache = TRUE))

  wac <- grab_lodes(
    state      = c("de", "vt"),
    year       = c(2017, 2019),
    version    = "LODES8",
    lodes_type = "wac",
    job_type   = "JT00",
    segment    = "S000",
    agg_geo    = "county"
  )

  result <- compute_lodes_change(
    wac,
    geo_col      = "w_county",
    base_year    = 2017,
    compare_year = 2019,
    variables    = c("C000", "CE01", "CE02", "CE03")
  )

  expect_s3_class(result, "tbl_df")
  for (suffix in c("_base", "_compare", "_change", "_pct_change")) {
    expect_true(
      paste0("C000", suffix) %in% names(result),
      info = paste0("Missing column: C000", suffix)
    )
  }
})

test_that("compute_lodes_change long output has required columns", {
  withr::local_options(list(lehdr_use_cache = TRUE))

  wac <- grab_lodes(
    state      = c("de", "vt"),
    year       = c(2017, 2019),
    version    = "LODES8",
    lodes_type = "wac",
    job_type   = "JT00",
    segment    = "S000",
    agg_geo    = "county"
  )

  result <- compute_lodes_change(
    wac,
    geo_col      = "w_county",
    base_year    = 2017,
    compare_year = 2019,
    variables    = "C000",
    output       = "long"
  )

  expect_s3_class(result, "tbl_df")
  expect_named(
    result,
    c("w_county", "variable", "base_year", "compare_year",
      "base_value", "compare_value", "change", "pct_change"),
    ignore.order = TRUE
  )
})

test_that("compute_lodes_change pct_change arithmetic is correct", {
  withr::local_options(list(lehdr_use_cache = TRUE))

  wac <- grab_lodes(
    state      = "de",
    year       = c(2018, 2020),
    version    = "LODES8",
    lodes_type = "wac",
    job_type   = "JT00",
    segment    = "S000",
    agg_geo    = "county"
  )

  result <- compute_lodes_change(
    wac,
    geo_col   = "w_county",
    variables = "C000"
  )

  # pct_change = (compare - base) / base * 100
  expected_pct <- (result$C000_compare - result$C000_base) / result$C000_base * 100
  expect_equal(result$C000_pct_change, expected_pct, tolerance = 1e-6)
})

test_that("compute_lodes_change defaults base and compare to min and max year", {
  withr::local_options(list(lehdr_use_cache = TRUE))

  wac <- grab_lodes(
    state      = "de",
    year       = c(2015, 2017, 2020),
    version    = "LODES8",
    lodes_type = "wac",
    job_type   = "JT00",
    segment    = "S000",
    agg_geo    = "county"
  )

  # Omitting base_year and compare_year should use 2015 and 2020
  result <- compute_lodes_change(wac, geo_col = "w_county", variables = "C000")

  expect_s3_class(result, "tbl_df")
  # In wide output there is no base_year column; verify via arithmetic identity
  # against manually filtered values
  base_vals    <- wac[wac$year == 2015, c("w_county", "C000")]
  compare_vals <- wac[wac$year == 2020, c("w_county", "C000")]
  merged       <- merge(base_vals, compare_vals, by = "w_county",
                        suffixes = c("_base", "_compare"))
  expect_equal(
    result$C000_change,
    merged$C000_compare - merged$C000_base
  )
})

test_that("compute_lodes_change auto-detects geo_col and emits an inform message", {
  withr::local_options(list(lehdr_use_cache = TRUE))

  wac <- grab_lodes(
    state      = "de",
    year       = c(2018, 2020),
    version    = "LODES8",
    lodes_type = "wac",
    job_type   = "JT00",
    segment    = "S000",
    agg_geo    = "county"
  )

  expect_message(
    compute_lodes_change(wac, variables = "C000"),
    "w_county"
  )
})

test_that("compute_lodes_change aborts when input has only one year", {
  single_year <- data.frame(w_county = "10001", C000 = 100L, year = 2020L)

  expect_error(
    compute_lodes_change(single_year, geo_col = "w_county"),
    "at least two distinct years"
  )
})

test_that("compute_lodes_change aborts when year column is absent", {
  no_year <- data.frame(w_county = "10001", C000 = 100L)

  expect_error(
    compute_lodes_change(no_year, geo_col = "w_county"),
    "`year` column"
  )
})

test_that("compute_lodes_change aborts when base_year is not in the data", {
  withr::local_options(list(lehdr_use_cache = TRUE))

  wac <- grab_lodes(
    state      = "de",
    year       = c(2018, 2020),
    version    = "LODES8",
    lodes_type = "wac",
    job_type   = "JT00",
    segment    = "S000",
    agg_geo    = "county"
  )

  expect_error(
    compute_lodes_change(wac, geo_col = "w_county", base_year = 2010),
    "2010"
  )
})

# ---- compute_earnings_share() ---------------------------------

test_that("compute_earnings_share WAC wide output shares sum to 1 per row", {
  withr::local_options(list(lehdr_use_cache = TRUE))

  wac <- grab_lodes(
    state      = "de",
    year       = 2020,
    version    = "LODES8",
    lodes_type = "wac",
    job_type   = "JT00",
    segment    = "S000",
    agg_geo    = "county"
  )

  result <- compute_earnings_share(wac, type = "wac", geo_col = "w_county")

  expect_s3_class(result, "tbl_df")
  expect_true(all(c("share_low", "share_mid", "share_high") %in% names(result)))
  row_sums <- result$share_low + result$share_mid + result$share_high
  expect_equal(row_sums, rep(1, nrow(result)), tolerance = 1e-6)
})

test_that("compute_earnings_share RAC wide output shares sum to 1 per row", {
  withr::local_options(list(lehdr_use_cache = TRUE))

  rac <- grab_lodes(
    state      = "de",
    year       = 2020,
    version    = "LODES8",
    lodes_type = "rac",
    job_type   = "JT00",
    segment    = "S000",
    agg_geo    = "county"
  )

  result <- compute_earnings_share(rac, type = "rac", geo_col = "h_county")

  expect_s3_class(result, "tbl_df")
  row_sums <- result$share_low + result$share_mid + result$share_high
  expect_equal(row_sums, rep(1, nrow(result)), tolerance = 1e-6)
})

test_that("compute_earnings_share long output has required columns", {
  withr::local_options(list(lehdr_use_cache = TRUE))

  wac <- grab_lodes(
    state      = "de",
    year       = 2020,
    version    = "LODES8",
    lodes_type = "wac",
    job_type   = "JT00",
    segment    = "S000",
    agg_geo    = "county"
  )

  result <- compute_earnings_share(
    wac,
    type    = "wac",
    geo_col = "w_county",
    output  = "long"
  )

  expect_s3_class(result, "tbl_df")
  expect_true(all(c("tier", "label", "count", "share") %in% names(result)))
  # 3 tiers * 3 DE counties = 9 rows
  expect_equal(nrow(result), 9L)
})

test_that("compute_earnings_share long shares are in [0, 1]", {
  withr::local_options(list(lehdr_use_cache = TRUE))

  wac <- grab_lodes(
    state      = "de",
    year       = 2020,
    version    = "LODES8",
    lodes_type = "wac",
    job_type   = "JT00",
    segment    = "S000",
    agg_geo    = "county"
  )

  result <- compute_earnings_share(
    wac,
    type    = "wac",
    geo_col = "w_county",
    output  = "long"
  )

  shares <- result$share[!is.na(result$share)]
  expect_true(all(shares >= 0 & shares <= 1))
})

test_that("compute_earnings_share auto-detects geo_col and emits an inform message", {
  withr::local_options(list(lehdr_use_cache = TRUE))

  wac <- grab_lodes(
    state      = "de",
    year       = 2020,
    version    = "LODES8",
    lodes_type = "wac",
    job_type   = "JT00",
    segment    = "S000",
    agg_geo    = "county"
  )

  expect_message(
    compute_earnings_share(wac, type = "wac"),
    "w_county"
  )
})

test_that("compute_earnings_share aborts when CE earnings columns are absent", {
  # Both WAC and RAC files include CE01/CE02/CE03 (earnings tiers). The
  # function should abort when those columns are missing, e.g. when the user
  # accidentally passes OD data or a non-S000 segment that lacks earnings cols.
  od_stub <- data.frame(
    w_tract = "10001001",
    h_tract = "10001002",
    S000    = 50L,
    year    = 2020L
  )

  expect_error(
    compute_earnings_share(od_stub, type = "wac", geo_col = "w_tract"),
    "CE0"
  )
})

test_that("compute_earnings_share aborts on invalid type argument", {
  stub <- data.frame(
    w_county = character(),
    CE01     = integer(),
    CE02     = integer(),
    CE03     = integer()
  )

  expect_error(
    compute_earnings_share(stub, type = "od"),
    "'arg' should be one of"
  )
})

test_that("compute_earnings_share count columns match raw column sums", {
  withr::local_options(list(lehdr_use_cache = TRUE))

  wac <- grab_lodes(
    state      = "de",
    year       = 2020,
    version    = "LODES8",
    lodes_type = "wac",
    job_type   = "JT00",
    segment    = "S000",
    agg_geo    = "county"
  )

  result <- compute_earnings_share(wac, type = "wac", geo_col = "w_county")

  # Verify raw sums for the first county in the result
  one_county <- wac[wac$w_county == result$w_county[1], ]
  expect_equal(result$count_low[1],  sum(one_county$CE01))
  expect_equal(result$count_mid[1],  sum(one_county$CE02))
  expect_equal(result$count_high[1], sum(one_county$CE03))
})
