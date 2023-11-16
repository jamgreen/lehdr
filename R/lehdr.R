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
#' @param use_cache Boolean indicating whether or not to check whether or not
#'   the file had already been downloaded. Defaults to FALSE. Setting to TRUE
#'   will allow lehdr to reuse existing files that were already downloaded.
#'
#' @description Download LODES OD, RAC, and WAC tables
#' @return a dataframe (tibble) of block or tract level LODES files
#' @import dplyr
#' @importFrom readr read_csv cols col_character
#' @importFrom httr GET stop_for_status HEAD write_disk
#' @importFrom glue glue
#' @importFrom stats na.omit
#' @importFrom stringr str_sub str_extract
#'  
#' @examples
#' \donttest{
#'  # download and load 2014 block level O-D data for Oregon
#'  blk_df_or_od <- grab_lodes(state = 'or', year = 2014, lodes_type = "od", job_type = "JT01", 
#'                          segment = "SA01", state_part = "main")
#'                          
#'  # download and load 2014 O-D data for Oregon and aggregate 
#'  # to the tract level                     
#'  trt_df_or_od <- grab_lodes(state = 'or', year = 2014, lodes_type = "od", job_type = "JT01", 
#'                          segment = "SA01", state_part = "main", agg_geo = "tract")
#'                          
#'  # download and load 2014 RAC data for Oregon and aggregate 
#'  # to the tract level                                              
#'  trt_df_or_rac <- grab_lodes(state = 'or', year = 2014, lodes_type = "rac", job_type = "JT01", 
#'                           segment = "SA01", agg_geo = "tract")
#'                           
#'  # download and load 2014 WAC data for Oregon and aggregate 
#'  # to the tract level                        
#'  trt_df_or_wac <- grab_lodes(state = 'or', year = 2014, lodes_type = "wac", job_type = "JT01", 
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
                       download_dir = file.path(tools::R_user_dir("lehdr", which="cache")),
                       use_cache = FALSE) { # Thanks Kyle Walker for this
  
  if (length(state) > 1 | length(year) > 1) {
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
    warning("state_part is required when setting lodes_type =\"od\", defaulting to state_part=\"main\"")
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
  
  # On URL error, the likely culprit is the lack of state/year combination ...
  httr::stop_for_status(httr::HEAD(url),
    paste("retrieve data for this combination of state and year on LODES.",
          "Please see the most recent LEHD Technical Document for a list of available state/year.",
          glue::glue("https://lehd.ces.census.gov/data/lodes/{version}/")
    )
  )
  
  # Set download directory, check for cache
  download_dir <- path.expand(download_dir)
  if (!dir.exists(download_dir))
    dir.create(download_dir, recursive=TRUE)
  fil <- file.path(download_dir, basename(url))
  
  
  if(use_cache) { # User set use_cache to TRUE
    # If there is a cache, use it
    if(file.exists(fil)) {
      message(glue::glue("Cached version of file found in: {fil}"))
    } else {
      message(glue::glue("Downloading {url} to cache folder: {fil}"))
      res <- httr::GET(url, httr::write_disk(fil))
      message(glue::glue("Download complete."))
    }
  } else { # User did not allow cache to be used
    if(file.exists(fil)) {
      # Existing file found, inform user of use_cache
      message(glue::glue("Cached version of file found in: {fil}"))
      message(glue::glue("Consider setting use_cache=TRUE to use previously downloaded files."))
    } else {
      # No file found, inform user that we're downloading
    }
    # Download (and overwite if necessary) data from server
    message(glue::glue("Overwriting {url} to {fil} now..."))
    res <- httr::GET(url, httr::write_disk(fil, overwrite = TRUE)) 
    message(glue::glue("Overwrite complete."))
  }
  
  # Read in the data
  lehdr_df <- suppressMessages(readr::read_csv(fil, col_types = col_types))
  
  # Remove temp files if the user did not set use_cache = TRUE
  if(!use_cache) {
    if(unlink(fil)) { # 0 for success, 1 for failure, invisibly. 
      message(glue::glue("Could not remove temporary file {fil}."))
    } else {
      message(glue::glue("Removed {fil}."))
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
    id_len <- switch(agg_geo_to, 
                     "bg" = 12, 
                     "tract" = 11, 
                     "county" = 5,
                     "state" = 2)
    lehdr_df <- aggregate_lodes_df(lehdr_df, id_len, agg_geo_to)
  }

  return(lehdr_df)
}

#' 
#' Aggregate data to a certain level dictated by inputs. Internal function.
#' 
#' @param lehdr_df Data frame (tibble) to be aggregated
#' @param geoid_to The number of characters to do the aggregation on from the geoid
#' @param aggname What the level is called, like "tract" etc
#' @description Helper function for lehdr which aggregates block geographies based on the block id.
#' @import dplyr
#' @importFrom glue glue
#' @importFrom stats na.omit
#' @importFrom stringr str_sub str_extract

aggregate_lodes_df <- function(lehdr_df, geoid_to, aggname) {
  lehdr_df <- lehdr_df %>% 
    mutate(across(ends_with("_geocode"),
              ~stringr::str_sub(., 1, geoid_to))) %>% 
    rename_with(~stringr::str_replace(., "geocode", aggname), 
                ends_with("_geocode")
              )
  
  ## Group by column(s) ending with "_{aggname}" as passed
  geoid_cols <- stringr::str_extract(names(lehdr_df), glue::glue(".*_{aggname}$")) %>% na.omit()
  group_cols <- c("year", "state", geoid_cols)
  
  lehdr_df <- lehdr_df %>%
    group_by(across({{group_cols}})) %>% 
    summarise_if(is.numeric, sum) %>% 
    ungroup()
  return(lehdr_df)
}

## quiets concerns of R CMD check re: non-standard evaluation via tidyverse
## per suggestion of Hadley Wickham (https://stackoverflow.com/a/12429344/688693)
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))