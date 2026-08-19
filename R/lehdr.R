#' Download and load LODES data into a data frame (tibble)
#'
#' @param state US state abbreviation in lower case, as character.
#'   Can be a vector of states, like `c("or", "md", "tx")` for Oregon,
#'   Maryland, and Texas. Two-letter FIPS abbreviations only.
#' @param year Year of the LODES data, as a numeric integer.
#'   Can be a vector of years, like `c(2014, 2020)` for 2014 and 2020.
#'   Must be between 2002 and 2023.
#' @param version The LODES version to use. `"LODES8"` (the default) is
#'   enumerated at 2020 Census blocks and covers 2002-2022. `"LODES7"` is
#'   enumerated at 2010 Census blocks and covers 2002-2019. `"LODES5"` is
#'   enumerated at 2000 Census blocks and covers 2002-2009.
#' @param lodes_type The LODES table type. Values can be the default
#'   origin-destination (`"od"`), residential area characteristics (`"rac"`),
#'   or workplace area characteristics (`"wac"`). OD files give a home and
#'   workplace census block for each worker flow. RAC files give job totals
#'   at worker home blocks; WAC files give job totals at worker job blocks.
#' @param job_type Job type segment: `"JT00"` for all jobs (default),
#'   `"JT01"` for Primary Jobs, `"JT02"` for All Private Jobs,
#'   `"JT03"` for Private Primary jobs, `"JT04"` for All Federal jobs,
#'   `"JT05"` for Federal Primary jobs.
#' @param segment Workforce segment. `"S000"` total jobs (default);
#'   `"SA01"` workers aged 29 or younger; `"SA02"` workers aged 30-54;
#'   `"SA03"` workers 55 and older; `"SE01"` earnings $1,250/month or less;
#'   `"SE02"` earnings $1,251-$3,333/month; `"SE03"` earnings above
#'   $3,333/month; `"SI01"` Goods Producing industries; `"SI02"` Trade,
#'   Transportation, & Utilities industries; `"SI03"` All Other Services.
#' @param state_part Part of the state file; required when
#'   `lodes_type = "od"`. `"main"` includes workers whose home and workplace
#'   are both in-state. `"aux"` includes workers who live out-of-state but
#'   work in the state of interest. Defaults to `"main"` with a warning when
#'   `lodes_type = "od"` and `state_part` is not explicitly supplied.
#' @param agg_geo Aggregate to a geography other than Census Block (default).
#'   Values can be `"bg"` (block group), `"tract"`, `"county"`, or
#'   `"state"`. The string `"block group"` is also accepted as an alias for
#'   `"bg"`.
#' @param download_dir Directory where the LODES file will be downloaded.
#'   Defaults to the user-level cache directory for `lehdr`.
#' @param geometry If `TRUE`, use the `tigris` package to download and attach
#'   spatial geometries from the U.S. Census Bureau for the specified `year`
#'   at the level specified by `agg_geo`. Returns an `sf` object when
#'   `lodes_type` is `"rac"` or `"wac"`.
#' @param use_cache Boolean. If `TRUE`, reuse previously downloaded files
#'   rather than re-downloading. Defaults to `FALSE`. You can also set the
#'   `lehdr_use_cache` global option to `TRUE` to make caching the default
#'   for a session.
#' @param ... Additional arguments passed to `tigris` functions when
#'   `geometry = TRUE`.
#'
#' @description Download LODES OD, RAC, and WAC tables from the LEHD FTP
#'   server and return a tidy data frame.
#'
#' @return A tibble of LODES data aggregated to block, block group, tract,
#'   county, or state level. If `geometry = TRUE` and `lodes_type` is
#'   `"rac"` or `"wac"`, returns an `sf` object.
#'
#' @import dplyr
#' @import httr2
#' @importFrom magrittr `%>%`
#' @importFrom readr read_csv cols col_character
#' @importFrom glue glue
#' @importFrom stats na.omit
#' @importFrom stringr str_sub str_extract
#'
#' @examples
#' \donttest{
#'   # Download 2014 block-level OD data for Vermont
#'   blk_od <- grab_lodes(
#'     state = "vt", year = 2014,
#'     lodes_type = "od", job_type = "JT01",
#'     segment = "SA01", state_part = "main"
#'   )
#'
#'   # Download 2014 OD data for Vermont aggregated to tract level
#'   trt_od <- grab_lodes(
#'     state = "vt", year = 2014,
#'     lodes_type = "od", job_type = "JT01",
#'     segment = "SA01", state_part = "main",
#'     agg_geo = "tract"
#'   )
#'
#'   # Download 2020 RAC data for Vermont aggregated to tract level
#'   trt_rac <- grab_lodes(
#'     state = "vt", year = 2020,
#'     lodes_type = "rac", job_type = "JT01",
#'     segment = "SA01", agg_geo = "tract"
#'   )
#'
#'   # Download 2020 WAC data for Vermont aggregated to tract level
#'   trt_wac <- grab_lodes(
#'     state = "vt", year = 2020,
#'     lodes_type = "wac", job_type = "JT01",
#'     segment = "SA01", agg_geo = "tract"
#'   )
#' }
#' @export
grab_lodes <- function(
  state,
  year,
  version    = c("LODES8", "LODES7", "LODES5"),
  lodes_type = c("od", "rac", "wac"),
  job_type   = c("JT00", "JT01", "JT02", "JT03", "JT04", "JT05"),
  segment    = c("S000", "SA01", "SA02", "SA03",
                 "SE01", "SE02", "SE03",
                 "SI01", "SI02", "SI03"),
  agg_geo    = c("block", "bg", "tract", "county", "state"),
  state_part = c("", "main", "aux"),
  download_dir = normalizePath(
    file.path(tools::R_user_dir("lehdr", which = "cache")),
    mustWork = FALSE
  ),
  geometry  = FALSE,
  use_cache = getOption("lehdr_use_cache", FALSE),
  ...
) {

  # ------------------------------------------------------------------
  # Validate use_cache early (consistent with rlang error style)
  # ------------------------------------------------------------------
  if (!is.logical(use_cache) || length(use_cache) != 1L) {
    rlang::abort(
      "The `use_cache` parameter must be a single logical value (TRUE or FALSE)."
    )
  }

  # ------------------------------------------------------------------
  # Vectorized dispatch: multiple states and/or years
  # ------------------------------------------------------------------
  if (length(state) > 1 || length(year) > 1) {
    state_year <- expand.grid(
      state = as.character(state),
      year  = as.integer(year),
      stringsAsFactors = FALSE
    )
    results <- lapply(seq_len(nrow(state_year)), function(i) {
      grab_lodes(
        state        = state_year$state[i],
        year         = state_year$year[i],
        version      = version,
        lodes_type   = lodes_type,
        job_type     = job_type,
        segment      = segment,
        agg_geo      = agg_geo,
        state_part   = state_part,
        download_dir = download_dir,
        use_cache    = use_cache
      )
    })
    return(dplyr::bind_rows(results))
  }

  # ------------------------------------------------------------------
  # Normalize scalar inputs
  # ------------------------------------------------------------------
  state   <- tolower(trimws(as.character(state)))
  year    <- as.integer(year)
  version <- toupper(version)

  # Allow "block group" as alias for "bg"
  if (identical(agg_geo, "block group")) {
    agg_geo <- "bg"
  }

  # ------------------------------------------------------------------
  # Validate state abbreviation
  # ------------------------------------------------------------------
  valid_states <- c(
    "al", "ak", "az", "ar", "ca", "co", "ct", "de", "dc", "fl",
    "ga", "hi", "id", "il", "in", "ia", "ks", "ky", "la", "me",
    "md", "ma", "mi", "mn", "ms", "mo", "mt", "ne", "nv", "nh",
    "nj", "nm", "ny", "nc", "nd", "oh", "ok", "or", "pa", "pr",
    "ri", "sc", "sd", "tn", "tx", "ut", "vt", "va", "wa", "wv",
    "wi", "wy", "vi", "gu", "mp", "as"
  )
  if (!state %in% valid_states) {
    rlang::abort(c(
      glue::glue("'{state}' is not a recognized state/territory FIPS abbreviation."),
      "i" = "Use a lowercase two-letter abbreviation, e.g. \"md\", \"ca\", \"pr\"."
    ))
  }

  # ------------------------------------------------------------------
  # Validate year range
  # ------------------------------------------------------------------
  year_range <- list(LODES8 = 2002:2022, LODES7 = 2002:2019, LODES5 = 2002:2009)
  # Use the first element of version for range-checking before arg_match
  ver_check <- toupper(version[1])
  if (!is.na(ver_check) && ver_check %in% names(year_range)) {
    if (!year %in% year_range[[ver_check]]) {
      rlang::abort(c(
        glue::glue(
          "`year` {year} is outside the valid range for {ver_check} \\
          ({min(year_range[[ver_check]])} - {max(year_range[[ver_check]])})."
        ),
        "i" = "See https://lehd.ces.census.gov/data/lodes/LODES8/ for availability."
      ))
    }
  }

  # ------------------------------------------------------------------
  # Match enumerated parameters
  # ------------------------------------------------------------------
  version    <- rlang::arg_match(version)
  lodes_type <- rlang::arg_match(lodes_type)
  agg_geo_to <- rlang::arg_match(agg_geo)
  job_type   <- rlang::arg_match(job_type)
  segment    <- rlang::arg_match(segment)
  state_part <- rlang::arg_match(state_part)

  # ------------------------------------------------------------------
  # Require state_part for OD; default to "main" with a warning
  # ------------------------------------------------------------------
  if (lodes_type == "od" && !state_part %in% c("main", "aux")) {
    state_part <- "main"
    rlang::warn(
      paste0(
        "`state_part` is required when `lodes_type = \"od\"`. ",
        "Defaulting to state_part = \"main\"."
      )
    )
  }

  # ------------------------------------------------------------------
  # Build URL and column type spec
  # ------------------------------------------------------------------
  if (lodes_type == "od") {
    url <- glue::glue(
      "https://lehd.ces.census.gov/data/lodes/{version}/{state}/",
      "{lodes_type}/{state}_{lodes_type}_{state_part}_{job_type}_{year}.csv.gz"
    )
    col_types <- cols(
      w_geocode  = col_character(),
      h_geocode  = col_character(),
      createdate = col_character()
    )
  } else {
    url <- glue::glue(
      "https://lehd.ces.census.gov/data/lodes/{version}/{state}/",
      "{lodes_type}/{state}_{lodes_type}_{segment}_{job_type}_{year}.csv.gz"
    )
    col_types <- if (lodes_type == "rac") {
      cols(h_geocode = col_character(), createdate = col_character())
    } else {
      cols(w_geocode = col_character(), createdate = col_character())
    }
  }

  # ------------------------------------------------------------------
  # Ensure download directory exists
  # ------------------------------------------------------------------
  if (!dir.exists(download_dir)) {
    dir.create(download_dir, recursive = TRUE)
  }
  fil <- normalizePath(
    file.path(download_dir, paste0(tolower(version), "_", basename(url))),
    mustWork = FALSE
  )

  # ------------------------------------------------------------------
  # Download (or use cache)
  # ------------------------------------------------------------------
  if (use_cache && file.exists(fil)) {
    rlang::inform(glue::glue("Using cached file: {basename(fil)}"))
  } else {
    if (use_cache) {
      rlang::inform(glue::glue("No cached file found; downloading {basename(fil)}..."))
    } else {
      if (file.exists(fil)) {
        rlang::inform(glue::glue(
          "Cached file found: {basename(fil)}. ",
          "Set `use_cache = TRUE` to reuse it."
        ))
      }
      rlang::inform(glue::glue("Downloading {url} to {basename(fil)}..."))
    }

    lodes_resp <- .download_lodes_file(url = url, path = fil)

    if (lodes_resp$status_code >= 400) {
      rlang::abort(c(
        glue::glue("Server error downloading: {url}"),
        "i" = glue::glue("HTTP {lodes_resp$status_code} returned."),
        "i" = paste0(
          "Consult the LEHD Technical Document to verify ",
          "state/year availability: ",
          "https://lehd.ces.census.gov/data/lodes/LODES8/"
        )
      ))
    } else if (length(lodes_resp$body) < 1) {
      rlang::abort(c(
        glue::glue("Empty response downloading: {url}"),
        "i" = "Check your internet connection."
      ))
    } else {
      rlang::inform(glue::glue("Download complete: {basename(fil)}"))
    }
  }

  # ------------------------------------------------------------------
  # Read data
  # ------------------------------------------------------------------
  lehdr_df <- suppressMessages(readr::read_csv(fil, col_types = col_types))

  # ------------------------------------------------------------------
  # Remove temp file unless caching
  # ------------------------------------------------------------------
  if (!use_cache) {
    if (unlink(fil) != 0L) {
      rlang::inform(glue::glue("Could not remove {basename(fil)} from cache."))
    } else {
      rlang::inform(glue::glue("{basename(fil)} cleared from cache."))
      if (length(list.files(download_dir)) == 0L) {
        unlink(download_dir, recursive = TRUE)
      }
    }
  }

  # ------------------------------------------------------------------
  # Attach year and state columns
  # ------------------------------------------------------------------
  lehdr_df <- lehdr_df %>%
    mutate(
      year  = year,
      state = toupper(state)
    )

  # ------------------------------------------------------------------
  # Aggregate geography if requested
  # ------------------------------------------------------------------
  if (agg_geo_to != "block" && !is.null(agg_geo_to)) {
    lehdr_df <- aggregate_lodes_df(lehdr_df, agg_geo_to)
  }

  # ------------------------------------------------------------------
  # Attach spatial geometry if requested
  # ------------------------------------------------------------------
  if (geometry) {
    lehdr_df <- join_lodes_geometry(
      lehdr_df   = lehdr_df,
      version    = version,
      agg_geo    = agg_geo_to,
      lodes_type = lodes_type,
      ...
    )
  }

  return(lehdr_df)
}


# ----------------------------------------------------------------------
# Internal: perform an httr2 download with consistent error handling
# @noRd
# ----------------------------------------------------------------------
.download_lodes_file <- function(url, path) {
  lodes_req <- request(url)
  withCallingHandlers(
    lodes_resp <- lodes_req |>
      req_error(is_error = \(r) FALSE) |>
      req_perform(path = path),
    httr2_failure = function(cnd) {
      rlang::abort(c(
        "lehdr: Could not connect to the LODES server.",
        "i" = "Please check your internet connection."
      ))
    }
  )
  lodes_resp
}


# ----------------------------------------------------------------------
# Subset GEOID string based on geo aggregation level
# @noRd
# ----------------------------------------------------------------------
st_sub_agg_geo <- function(string, agg_geo, start = 1) {
  end <- switch(
    agg_geo,
    "block"  = -1,
    "bg"     = 12,
    "tract"  = 11,
    "county" = 5,
    "state"  = 2
  )
  stringr::str_sub(string, start = start, end = end)
}


# ----------------------------------------------------------------------
# Internal: download spatial data and join to LEHDR data frame
# @noRd
# ----------------------------------------------------------------------
join_lodes_geometry <- function(
  lehdr_df,
  version,
  agg_geo,
  lodes_type,
  ...
) {
  rlang::check_installed("sf")

  lehdr_sf <- grab_lodes_geometry(
    lehdr_df = lehdr_df,
    version  = version,
    agg_geo  = agg_geo,
    ...
  )

  if (lodes_type %in% c("od", "wac")) {
    sf_column_name <- "w_geometry"
    lehdr_df <- join_lehdr_sf_obj(
      lehdr_df,
      lehdr_sf,
      geoid_col      = agg_geo_col(agg_geo, "w"),
      sf_column_name = sf_column_name
    )
  }

  if (lodes_type %in% c("od", "rac")) {
    sf_column_name <- "h_geometry"
    lehdr_df <- join_lehdr_sf_obj(
      lehdr_df,
      lehdr_sf,
      geoid_col      = agg_geo_col(agg_geo, "h"),
      sf_column_name = sf_column_name
    )
  }

  if (lodes_type != "od") {
    lehdr_df <- sf::st_as_sf(lehdr_df, sf_column_name = sf_column_name)
  } else {
    rlang::inform(c(
      '`grab_lodes()` returns a non-sf object when `geometry = TRUE` and `lodes_type = "od"`.',
      "*" = paste0(
        "Use `sf::st_as_sf()` with `sf_column_name` set to ",
        "`\"w_geometry\"` or `\"h_geometry\"` to convert."
      )
    ))
  }

  lehdr_df
}


# ----------------------------------------------------------------------
# Internal: left-join an sf object to a data frame using GEOID
# @noRd
# ----------------------------------------------------------------------
join_lehdr_sf_obj <- function(
  df_obj,
  sf_obj,
  geoid_col,
  sf_column_name = "w_geometry",
  .f = dplyr::left_join
) {
  sf_obj <- sf::st_set_geometry(sf_obj, value = sf_column_name)
  by     <- rlang::set_names("GEOID", nm = geoid_col)
  .f(df_obj, dplyr::as_tibble(sf_obj), by = by)
}


# ----------------------------------------------------------------------
# Internal: get the Census year corresponding to a LODES version
# @noRd
# ----------------------------------------------------------------------
get_lodes_version_year <- function(version) {
  switch(version, LODES8 = 2020, LODES7 = 2010, LODES5 = 2000)
}


# ----------------------------------------------------------------------
# Internal: download spatial geometries via tigris
# @noRd
# ----------------------------------------------------------------------
grab_lodes_geometry <- function(
  lehdr_df = NULL,
  version  = NULL,
  agg_geo  = NULL,
  state    = NULL,
  county   = NULL,
  ...
) {
  rlang::check_installed("tigris")

  year <- get_lodes_version_year(version)

  geoid_values <- unique(c(
    lehdr_df[[agg_geo_col(agg_geo, "w")]],
    lehdr_df[[agg_geo_col(agg_geo, "h")]]
  ))

  if (is.null(state)) {
    if (agg_geo %in% c("block", "bg", "tract") && is.null(county)) {
      geoid_values <- unique(st_sub_agg_geo(geoid_values, "county"))
      state        <- st_sub_agg_geo(geoid_values, "state")
      county       <- st_sub_agg_geo(geoid_values, "county", start = 3)
    } else {
      state <- unique(st_sub_agg_geo(geoid_values, "state"))
    }
  }

  tigris_fn <- switch(
    agg_geo,
    block  = function(state = NULL, county = NULL, year = NULL, ...) {
      tigris::blocks(state = state, county = county, year = year, ...)
    },
    bg     = function(state = NULL, county = NULL, year = NULL, ...) {
      tigris::block_groups(state = state, county = county, year = year, ...)
    },
    tract  = function(state = NULL, county = NULL, year = NULL, ...) {
      tigris::tracts(state = state, county = county, year = year, ...)
    },
    county = tigris::counties,
    state  = function(state = NULL, year = NULL, ...) {
      tigris_geo <- tigris::states(year = year, ...)
      dplyr::filter(tigris_geo, STATEFP %in% state)
    }
  )

  if (agg_geo == "state") {
    lehdr_sf_list <- list(tigris_fn(state = state, year = year, ...))
  } else if (agg_geo %in% c("block", "bg", "tract")) {
    lehdr_sf_list <- mapply(
      tigris_fn, state, county, year, ...,
      SIMPLIFY = FALSE
    )
  } else {
    lehdr_sf_list <- lapply(state, function(st) {
      tigris_fn(state = st, year = year, ...)
    })
  }

  lehdr_sf <- dplyr::bind_rows(lehdr_sf_list)
  cols <- "GEOID"

  if (agg_geo == "block") {
    cols <- intersect(
      names(lehdr_sf),
      c("BLKIDFP00", "BLKIDFP10", "BLKIDFP20",
        "GEOID00", "GEOID10", "GEOID20")
    )[1]
    cols <- rlang::set_names(cols, "GEOID")
  } else if (agg_geo == "county" && !rlang::has_name(lehdr_sf, "GEOID")) {
    lehdr_sf <- dplyr::mutate(lehdr_sf, GEOID = paste0(STATEFP, COUNTYFP))
  }

  dplyr::select(lehdr_sf, dplyr::all_of(cols))
}


# ----------------------------------------------------------------------
# Internal: build the geocode column name for a given geo + prefix
# @noRd
# ----------------------------------------------------------------------
agg_geo_col <- function(agg_geo, before = "w") {
  col_suffix <- if (agg_geo == "block") "geocode" else agg_geo
  glue::glue("{before}_{col_suffix}")
}


#' Aggregate LODES data to a coarser geographic level
#'
#' @param lehdr_df Data frame (tibble) of LODES data at the block level.
#' @param agg_geo Target geography, e.g. `"tract"`, `"county"`, `"state"`,
#'   `"bg"`.
#' @description Internal helper that truncates GEOID strings and sums
#'   numeric columns.
#' @importFrom dplyr mutate across group_by summarise ungroup
#' @importFrom glue glue
#' @importFrom stats na.omit
#' @importFrom stringr str_extract str_replace
#' @noRd
aggregate_lodes_df <- function(lehdr_df, agg_geo) {
  lehdr_df <- lehdr_df %>%
    mutate(across(
      ends_with("_geocode"),
      ~ st_sub_agg_geo(., agg_geo = agg_geo)
    )) %>%
    rename_with(
      ~ stringr::str_replace(., "geocode", agg_geo),
      ends_with("_geocode")
    )

  geoid_cols <- stringr::str_extract(
    names(lehdr_df),
    glue::glue(".*_{agg_geo}$")
  ) %>% na.omit()

  group_cols <- c("year", "state", geoid_cols)

  lehdr_df %>%
    group_by(across({{ group_cols }})) %>%
    summarise(across(where(is.numeric), sum), .groups = "drop")
}


## quiets R CMD check NOTEs for non-standard evaluation in tidyverse
utils::globalVariables(c(".", "COUNTYFP", "GEOID", "STATEFP"))
