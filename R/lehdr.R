#'
#' Download and load LODES data into a data frame (tibble)
#'
#' @param state US state abbreviation in lower case, as character. 
#'   Can be a vector of states, like c("or","md","tx") for Oregon, Maryland, 
#'   and Texas.
#' @param year Year of the lodes data, as numerical.
#'   Can be a vector of years, like c(2014, 2020) for 2014 and 2020.
#' @param version The LODES version to use.  
#'   Version 8 (the default, use "LODES8") is enumerated at 2020 Census blocks. 
#'   "LODES7" is enumerated at 2010 Census blocks, but ends in 2019; 
#'   LODES5" is enumerated at 2000 Census blocks, but ends in 2009.  
#' @param lodes_type The LODES table type.
#'   Values can be the default origin-destination ("od"),residential association
#'   ("rac"), or workplace association ("wac"). 
#'   od files give a home and destination census block for workers. Residential 
#'   files give job totals of worker home census blocks and workplace files 
#'   give job totals of worker job census blocks
#' @param job_type Jobtype: "JT00" for all jobs, "JT01" for Primary Jobs, 
#'   "JT02" for All Private Jobs, "JT03" for Private Primary jobs, "JT04" for
#'   All Federal jobs, "JT05" for Federal Primary jobs
#' @param segment Segment of the workforce.
#'   "S000" total number of jobs for workers, "SA01" number of jobs for workers 
#'   aged 29 or younger, "SA02" number of jobs for workers aged 30-54,"SA03" 
#'   number of jobs for workers 55 and older, "SE01" number of jobs with 
#'   earnings $1,250/month or less, "SE02" number of jobs with earnings $1,251 
#'   to $3,333/month, "SE03" number of jobs with earnings greater than 
#'   $3,333/month, "SI01" number of jobs in Goods Producing industry sectors, 
#'   "SI02" number of jobs in Trade, Transportation, & Utilities industry 
#'   sectors, "SI03" number of jobs in All Other Services industry sectors.
#' @param state_part Part of the state file, required for "od" lodes_type.  
#'   Can have values of "main" or "aux" in OD files. Using "main" includes 
#'   workers with their workplace and residence in the state. Using "aux" 
#'   includes workers with residences out of state and workplace in the state 
#'   of interest
#' @param agg_geo Aggregate to a geography other than Census Block (default). 
#'   Values can be "bg" for block group, "tract", "county", or "state".
#' @param download_dir Directory where lodes table will be downloaded.
#' @param geometry If `TRUE`, use the tigris package to download spatial data
#'   from the U.S. Census Bureau for the specified `year` and `geometry_state`
#'   at the level specified by `agg_geo`.
#' @param use_cache Boolean indicating whether or not to check whether or not
#'   the file had already been downloaded. Defaults to FALSE. Setting to TRUE or
#'   setting the `lehdr_use_cache` option to TRUE will allow lehdr to reuse
#'   existing files that were already downloaded.
#' @param ... Additional arguments passed to tigris functions for data access.
#' @description Download LODES OD, RAC, and WAC tables
#' @return A dataframe (tibble) of LODES data aggregated to a block, block
#'   group, tract, county, or state level. If `geometry = TRUE` and `lodes_type`
#'   is "rac" or "wac" return a sf object.
#' @import dplyr
#' @import magrittr
#' @import httr2
#' @importFrom readr read_csv cols col_character
#' @importFrom glue glue
#' @importFrom stats na.omit
#' @importFrom stringr str_sub str_extract
#'  
#' @examples
#' \donttest{
#'  # download and load 2014 block level O-D data for Vermont
#'  blk_df_or_od <- grab_lodes(state = 'vt', year = 2014, lodes_type = "od", job_type = "JT01", 
#'                          segment = "SA01", state_part = "main")
#'                          
#'  # download and load 2014 O-D data for Vermont and aggregate 
#'  # to the tract level                     
#'  trt_df_or_od <- grab_lodes(state = 'vt', year = 2014, lodes_type = "od", job_type = "JT01", 
#'                          segment = "SA01", state_part = "main", agg_geo = "tract")
#'                          
#'  # download and load 2020 RAC data for Vermont and aggregate 
#'  # to the tract level                                              
#'  trt_df_or_rac <- grab_lodes(state = 'vt', year = 2014, lodes_type = "rac", job_type = "JT01", 
#'                           segment = "SA01", agg_geo = "tract")
#'                           
#'  # download and load 2020 WAC data for Vermont and aggregate 
#'  # to the tract level                        
#'  trt_df_or_wac <- grab_lodes(state = 'vt', year = 2014, lodes_type = "wac", job_type = "JT01", 
#'                           segment = "SA01", agg_geo = "tract")
#' }                         
#' @export
grab_lodes <- function(state, year, 
                       version = c("LODES8", "LODES7", "LODES5"),
                       lodes_type = c("od", "rac", "wac"),
                       job_type = c("JT00", "JT01", "JT02", "JT03", "JT04", "JT05"), 
                       segment = c("S000", "SA01", "SA02", "SA03", "SE01", "SE02",
                                   "SE03", "SI01", "SI02", "SI03"),  
                       agg_geo = c("block", "bg", "tract", "county", "state"),
                       state_part = c("","main","aux"), 
                       download_dir = normalizePath(file.path(tools::R_user_dir("lehdr", which="cache")), 
                                                    mustWork = FALSE),
                       geometry = FALSE,
                       use_cache = getOption("lehdr_use_cache", FALSE),
                       ...) {
  
  if (length(state) > 1 || length(year) > 1) {
    ## Handle multiple states x years
    state_year <- expand.grid(state=state, year=year)
    results <- apply(state_year, 1, function(df_row) grab_lodes(df_row[1], df_row[2], 
                                                           version = version,
                                                           lodes_type = lodes_type,
                                                           job_type = job_type,
                                                           segment = segment,
                                                           agg_geo = agg_geo,
                                                           state_part = state_part,
                                                           download_dir = download_dir) 
                     )
    return(dplyr::bind_rows(results))
  }
  
  # Format inputs
  state <- tolower(state)
  version <- toupper(version)
  
  # Handle errors and set default arguments
  version <- rlang::arg_match(version)
  lodes_type <- rlang::arg_match(lodes_type)
  agg_geo_to <- rlang::arg_match(agg_geo)
  job_type <- rlang::arg_match(job_type)
  segment <- rlang::arg_match(segment)
  state_part <- rlang::arg_match(state_part)
  
  # Only proceed if use_cache is boolean
  if(!is.logical(use_cache)) { stop("The use_cache paramater must be either TRUE or FALSE") }
  
  # If someone uses od, but doesn't set state_part
  if(lodes_type == "od" && !(state_part %in% c("main","aux"))) {
    state_part <- "main"
    warning("'state_part' is required when setting lodes_type =\"od\", defaulting to state_part=\"main\"")
  }
  
  # Setup col_types and url variables, used in the if statement
  cols_types <- cols()
  url <- as.character("")
  
  # Read LODES_types and set col_types
  if (lodes_type == "od") {
    # Set url for od
    url <- glue::glue("https://lehd.ces.census.gov/data/lodes/{version}/{state}/{lodes_type}/{state}_{lodes_type}_{state_part}_{job_type}_{year}.csv.gz")
    # Set column types for od
    col_types <- cols(w_geocode = col_character(), 
                      h_geocode = col_character(),
                      createdate = col_character())
  } else {
    # Set url for rac/wac
    url <- glue::glue("https://lehd.ces.census.gov/data/lodes/{version}/{state}/{lodes_type}/{state}_{lodes_type}_{segment}_{job_type}_{year}.csv.gz")    

    # Set column types for rac/wac -- h_ is home, w_ is work (from LODES)
    if (lodes_type == "rac") {
      col_types <- cols(h_geocode = col_character(),
                        createdate = col_character())
    } else if (lodes_type == "wac") {
      col_types <- cols(w_geocode = col_character(), 
                        createdate = col_character())
    }
  }
  
  # Set download directory and file name
  if (!dir.exists(download_dir)) {
    dir.create(download_dir, recursive=TRUE)
  }
  fil <- normalizePath(file.path(download_dir, basename(url)), mustWork = FALSE)
  
  # Create httr2 request from url
  lodes_req <- request(url)
  
  # Check for cache
  if(use_cache) { # User set use_cache to TRUE
    if(file.exists(fil)) { # If there is a cached file, use it
      rlang::inform(glue::glue("Using cached version of file found in: {fil}"))
    } else { # No cached file
      # Perform request and handle connection errors, writing response to disk
      withCallingHandlers(
        lodes_resp <- lodes_req |>
          req_error(is_error = \(lodes_resp) FALSE) |>
          req_perform(path = fil),
        httr2_failure = function(cnd) {
          rlang::abort(c("lehdr: Could not establish connection to LODES FTP server.", 
                         "i" = "Please check internet connection."))
        }
      )
      if(lodes_resp$status_code >= 400) {
        rlang::abort(c(glue::glue("lehdr: Server error while downloading: {lodes_req$url}"),
                       "i" = glue::glue("{lodes_resp$status_code} status returned."),
                       "i" = "Please consult the most recent LEHD Technical Document to verify state/year combination availability.",
                       "i" = "https://lehd.ces.census.gov/data/lodes/8/"))
      } else if(length(lodes_resp$body) < 1) {
        rlang::abort(c(glue::glue("lehdr: Connection error while downloading: {lodes_req$url}"),
                       "i" = "Empty response."))
      } else {
        rlang::inform(glue::glue("Download complete for {fil}"))
      }
    }
  } else { # User did not allow cache to be used
    if(file.exists(fil)) { # But there is a cached file
      # Existing file found, inform user of use_cache
      rlang::inform(glue::glue("Cached version of file found in: {fil}"))
      rlang::inform(glue::glue("Consider setting use_cache=TRUE to use previously downloaded files."))
    }
    # Download (and overwite if necessary) data from server
    rlang::inform(glue::glue("Downloading {url} to {fil} now..."))
    
    # Perform request and handle connection errors, writing response to disk
    withCallingHandlers(
      lodes_resp <- lodes_req |>
        req_error(is_error = \(lodes_resp) FALSE) |>
        req_perform(path = fil),
      httr2_failure = function(cnd) {
        rlang::abort(c("lehdr: Could not establish connection to LODES FTP server.", 
                     "i" = "Please check internet connection."))
      }
    )
    if(lodes_resp$status_code >= 400) {
      rlang::abort(c(glue::glue("lehdr: Server error while downloading: {lodes_req$url}"),
                     "i" = glue::glue("{lodes_resp$status_code} status returned."),
                     "i" = "Please consult the most recent LEHD Technical Document to verify state/year combination availability.",
                     "i" = "https://lehd.ces.census.gov/data/lodes/8/"))
    } else if(length(lodes_resp$body) < 1) {
      rlang::abort(c(glue::glue("lehdr: Connection error while downloading: {lodes_req$url}"),
                     "i" = "Empty response."))
    } else {
      rlang::inform(glue::glue("Download complete for {fil}"))
    }
  }
  
  # Read in the data
  lehdr_df <- suppressMessages(readr::read_csv(fil, col_types = col_types))
  
  # Remove temp files if the user did not set use_cache = TRUE
  if(!use_cache) {
    if(unlink(fil)) { # 0 for success, 1 for failure, invisibly. 
      rlang::inform(glue::glue("Could not clear {fil} from cache."))
    } else {
      rlang::inform(glue::glue("{fil} cleared from cache."))
      # Now check to see if the cache directory is empty, remove it if it is
      if(length(list.files(download_dir)) == 0) {
        unlink(download_dir, recursive = TRUE)
      }
    }
  }

  # Mutate the data based on year and state
  lehdr_df <- lehdr_df %>% mutate(
    year=year,
    state=toupper(state))
  
  # Group by geography
  # If the agg_geo_to variable is "block" or NULL (not passed via args), 
  # then skip this step as the data is already in block format
  if(agg_geo_to != "block" && !is.null(agg_geo_to)) { 
    lehdr_df <- aggregate_lodes_df(lehdr_df, agg_geo_to)
  }
  
  if (geometry) {
    lehdr_df <- join_lodes_geometry(
      lehdr_df = lehdr_df,
      version = version,
      agg_geo = agg_geo_to,
      lodes_type = lodes_type,
      ...
    )
  }

  return(lehdr_df)
}

#' Subset GEOID string based on geo aggregation level
#' @noRd
st_sub_agg_geo <- function(string, agg_geo, start = 1) {
  end <- switch(
    agg_geo,
    "block" = -1,
    "bg" = 12,
    "tract" = 11,
    "county" = 5,
    "state" = 2
  )
  stringr::str_sub(string, start = start, end = end)
}

#' Internal function to download and join spatial data to LEHDR data frame
#' @noRd
join_lodes_geometry <- function(
    lehdr_df,
    version,
    agg_geo,
    lodes_type,
    ...) {
  rlang::check_installed("sf")

  # Download 
  lehdr_sf <- grab_lodes_geometry(
    lehdr_df = lehdr_df,
    version = version,
    agg_geo = agg_geo,
    ...
  )
  
  if (lodes_type %in% c("od", "wac")) {
    sf_column_name <- "w_geometry"
    lehdr_df <- join_lehdr_sf_obj(
      lehdr_df,
      lehdr_sf,
      geoid_col = agg_geo_col(agg_geo, "w"),
      sf_column_name = sf_column_name
    )
  }
  
  if (lodes_type %in% c("od", "rac")) {
    sf_column_name <- "h_geometry"
    lehdr_df <- join_lehdr_sf_obj(
      lehdr_df,
      lehdr_sf,
      geoid_col = agg_geo_col(agg_geo, "h"),
      sf_column_name = sf_column_name
    )
  }
  
  if (lodes_type != "od") {
    lehdr_df <- sf::st_as_sf(lehdr_df, sf_column_name = sf_column_name)
  } else {
    rlang::inform(
      c(
        '`get_lodes()` returns a non-sf object when `geometry = TRUE` and `lodes_type = "od"`.',
        "*" = "Use `sf::st_as_sf` with sf_column_name set to `w_geometry` or `h_geometry` to convert data to an sf object."
      )
    )
  }
  
  lehdr_df
}

#' Join sf object to a data frame using geoid column as `by`
#' @noRd
join_lehdr_sf_obj <- function(
    df_obj,
    sf_obj,
    geoid_col,
    sf_column_name = "w_geometry",
    .f = dplyr::left_join) {
  sf_obj <- sf::st_set_geometry(sf_obj, value = sf_column_name)
  by <- rlang::set_names("GEOID", nm = geoid_col)
  .f(df_obj, dplyr::as_tibble(sf_obj), by = by)
}



#' Get year from LODES version
#' @noRd
get_lodes_version_year <- function(version) {
  switch (version,
          LODES8 = 2020,
          LODES7 = 2010,
          LODES5 = 2000
  )
}

#' Grab spatial data to LEHDR data frame
#' @noRd
grab_lodes_geometry <- function(
  lehdr_df = NULL,
  version = NULL,
  agg_geo = NULL,
  state = NULL,
  county = NULL,
  ...
) {
  rlang::check_installed("tigris")

  # Get year for version
  year <- get_lodes_version_year(version)

  # Get GeoID values
  geoid_values <- unique(c(
    lehdr_df[[agg_geo_col(agg_geo, "w")]],
    lehdr_df[[agg_geo_col(agg_geo, "h")]]
  ))

  # TODO: Validate state and convert to FIPS code if non-FIPS code input is
  # provided

  if (agg_geo %in% c("block", "bg", "tract") && is.null(county)) {
    geoid_values <- unique(st_sub_agg_geo(geoid_values, "county"))
    state <- st_sub_agg_geo(geoid_values, "state")
    county <- st_sub_agg_geo(geoid_values, "county", start = 3)
  } else if (is.null(state)) {
    # Get states FIPS codes from input LODES data
    state <- st_sub_agg_geo(geoid_values, "state")
    state <- unique(state)
  }

  # Set tigris function based on agg_geo
  tigris_fn <- switch(
    agg_geo,
    block = function(state = NULL, county = NULL, year = NULL, ...) {
      tigris::blocks(state = state, county = county, year = year, ...)
    },
    bg = function(state = NULL, county = NULL, year = NULL, ...) {
      tigris::block_groups(state = state, county = county, year = year, ...)
    },
    tract = function(state = NULL, county = NULL, year = NULL, ...) {
      tigris::tracts(state = state, county = county, year = year, ...)
    },
    county = tigris::counties,
    state = function(state = NULL, year = NULL, ...) {
      tigris_geo <- tigris::states(year = year, ...)
      
      dplyr::filter(
        tigris_geo,
        STATEFP %in% state
      )
    } 
  )

  if (agg_geo == "state") {
    # Download all states
    lehdr_sf_list <- list(tigris_fn(state = state, year = year, ...))
  } else if (agg_geo %in% c("block", "bg", "tract")) {
    # Download block, block group, and tract data for all counties
    lehdr_sf_list <- mapply(
      tigris_fn,
      state,
      county,
      year,
      ...,
      SIMPLIFY = FALSE
    )
  } else {
    # Download county data from tigris for each state
    lehdr_sf_list <- lapply(
      state,
      function(st) {
        tigris_fn(
          state = st,
          year = year,
          ...
        )
      }
    )
  }

  lehdr_sf <- dplyr::bind_rows(lehdr_sf_list)
  cols <- "GEOID"

  if (agg_geo == "block") {
    # Handle variant GEOID column names for block-level data
    cols <- switch(
      as.character(year),
      "2020" = "GEOID20",
      "2010" = "GEOID10",
      "2000" = "BLKIDFP00"
    )

    cols <- rlang::set_names(cols, "GEOID")
    print(lehdr_sf)
  } else if (agg_geo == "county" && !rlang::has_name(lehdr_sf, "GEOID")) {
    # Handle missing GEOID column for county-level data
    lehdr_sf <- dplyr::mutate(
      lehdr_sf,
      GEOID = paste0(STATEFP, COUNTYFP)
    )
  }

  dplyr::select(lehdr_sf, dplyr::all_of(cols))
}

#' @noRd
agg_geo_col <- function(agg_geo, before = "w") {
  col_suffix <- agg_geo
  if (agg_geo == "block") {
    col_suffix <- "geocode"
  }
  
  glue::glue("{before}_{col_suffix}")
}

#' Aggregate data to a certain level dictated by inputs. Internal function.
#' 
#' @param lehdr_df Data frame (tibble) to be aggregated
#' @param agg_geo What the level is called, like "tract" etc
#' @description Helper function for lehdr which aggregates block geographies based on the block id.
#' @import dplyr
#' @importFrom glue glue
#' @importFrom stats na.omit
#' @importFrom stringr str_sub str_extract

aggregate_lodes_df <- function(lehdr_df, agg_geo) {
  lehdr_df <- lehdr_df %>% 
    mutate(across(ends_with("_geocode"),
              ~st_sub_agg_geo(., agg_geo = agg_geo))) %>% 
    rename_with(~stringr::str_replace(., "geocode", agg_geo), 
                ends_with("_geocode")
              )
  
  ## Group by column(s) ending with "_{agg_geo}" as passed
  geoid_cols <- stringr::str_extract(names(lehdr_df), glue::glue(".*_{agg_geo}$")) %>% na.omit()
  group_cols <- c("year", "state", geoid_cols)
  
  lehdr_df <- lehdr_df %>%
    group_by(across({{group_cols}})) %>% 
    summarise_if(is.numeric, sum) %>% 
    ungroup()
  return(lehdr_df)
}

## quiets concerns of R CMD check re: non-standard evaluation via tidyverse
## per suggestion of Hadley Wickham (https://stackoverflow.com/a/12429344/688693)
utils::globalVariables(c(".", "COUNTYFP", "GEOID", "STATEFP"))