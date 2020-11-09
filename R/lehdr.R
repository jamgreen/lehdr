#'
#' Download and load LODES data into a data frame (tibble)
#'
#' @param state US state abbreviation in lower case, can be a vector of states.
#' @param year year of the lodes data, can be a vector of years.
#' @param lodes_type table type, values can be origin-destination ("od"), 
#'   residential association ("rac"), or workplace association ("wac"). od 
#'   files give a home and destination census block for workers. Residential 
#'   files give job totals of worker home census blocks and workplace files 
#'   give job totals of worker job census blocks
#' @param job_type Jobtype: "JT00" for all jobs, "JT01" for Primary Jobs, 
#'   "JT02" for All Private Jobs, "JT03" for Private Primary jobs, "JT04" for
#'   All Federal jobs, "JT05" for Federal Primary jobs
#' @param segment Segment of the workforce: "S000" total number of jobs for
#'   workers, "SA01" number of jobs forworker aged 29 or younger, "SA02" number
#'   of jobs for workers aged 30-54,"SA03" number of jobs for workers 55 and
#'   older, "SE01" number of jobs with earnings $1,250/month or less, "SE02"
#'   number of jobs with earnings $1,251 to $3,333/month, "SE03" number of jobs
#'   with earnings greater than $3,333/month, "SI01" number of jobs in Goods
#'   Producing industry sectors, "SI02" number of jobs in Trade, 
#'   Transportation, and Utilities industry sectors, "SI03" number of jobs in 
#'   All Other Services industry sectors
#' @param state_part Part of the state file, can have values of "main" or "aux"
#'   in OD files. Using "main" includes workers with their workplace and
#'   residence in the state. Using "aux" includes workers with residences out 
#'   of state and workplace in the state of interest
#' @param agg_geo Aggregate to a geography other than Census Block (default). 
#'   Values can be "bg" for block group, "tract", "county", or "state".
#' @param download_dir Directory where lodes table will be downloaded.
#'
#' @description Download LODES OD, RAC, and WAC tables
#' @return a dataframe (tibble) of block or tract level LODES files
#' @import dplyr
#' @importFrom rappdirs user_cache_dir
#' @importFrom readr read_csv cols col_character
#' @importFrom httr GET stop_for_status HEAD write_disk
#' @importFrom glue glue
#' @importFrom stats na.omit
#' @importFrom stringr str_sub str_extract
#'  
#' @examples
#' \dontrun{
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
                       lodes_type = c("od", "rac", "wac"),
                       job_type = c("JT00", "JT01", "JT02", "JT03", "JT04", "JT05"), 
                       segment = c("S000", "SA01", "SA02", "SA03", "SE01", "SE02",
                                   "SE03", "SI01", "SI02", "SI03"),  
                       agg_geo = c("block", "bg", "tract", "county", "state"),
                       state_part = c("","main","aux"), 
                       download_dir = file.path(rappdirs::user_cache_dir(appname="lehdr")) ) {
  
  if (length(state) > 1 | length(year) > 1) {
    ## Handle multiple states x years
    state_year <- expand.grid(state=state, year=year)
    results <- apply(state_year, 1, function(df_row) grab_lodes(df_row[1], df_row[2], 
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
  lodes_type <- match.arg(tolower(lodes_type), c(NULL, "od", "rac", "wac"))
  agg_geo_to <- match.arg(tolower(agg_geo), c(NULL, "block", "bg", "tract", "county", "state"))
  job_type <- match.arg(job_type, c(NULL, "JT00", "JT01", "JT02", "JT03", "JT04", "JT05"))
  segment <- match.arg(segment, c(NULL, "S000", "SA01", "SA02", "SA03", "SE01", "SE02","SE03", "SI01", "SI02", "SI03"))
  state_part <- match.arg(state_part, c("","main","aux"))
  
  # If someone uses od, but doesn't set state_part
  if(lodes_type == "od" && state_part == "") {
    state_part == "main"
    warning("state_part is required when setting lodes_type =\"od\", defaulting to state_part=\"main\"")
  }
  
  # Setup col_types and url variables, used in the if statement
  cols_types <- cols()
  url <- as.character("")
  
  # Read LODES_types and set col_types
  if (lodes_type == "od") {
    # Set url for od
    url <- glue::glue("https://lehd.ces.census.gov/data/lodes/LODES7/{state}/{lodes_type}/{state}_{lodes_type}_{state_part}_{job_type}_{year}.csv.gz")
    # Set column types for od
    col_types <- cols(w_geocode = col_character(), 
                      h_geocode = col_character(),
                      createdate = col_character())
  } else {
    # Set url for rac/wac
    url <- glue::glue("https://lehd.ces.census.gov/data/lodes/LODES7/{state}/{lodes_type}/{state}_{lodes_type}_{segment}_{job_type}_{year}.csv.gz")    

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
          "https://lehd.ces.census.gov/data/lodes/LODES7/"
    )
  )
  
  # Set download directory, check for cache
  download_dir <- path.expand(download_dir)
  if (!dir.exists(download_dir))
    dir.create(download_dir, recursive=TRUE)
  fil <- file.path(download_dir, basename(url))
  
  # If there is a cache, use it
  if(file.exists(fil)) {
    message(glue::glue("Cached version of file found in {fil}\n Reading now..."))
  } else {
    message(glue::glue("Downloading {url} to {fil} now..."))
    res <- httr::GET(url, httr::write_disk(fil))
  }
  
  # Read in the data
  lehdr_df <- suppressMessages(readr::read_csv(fil, col_types = col_types))

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
    mutate_at(vars(ends_with("_geocode")),
              funs(stringr::str_sub(., 1, geoid_to))) %>% 
    rename_at(vars(ends_with("_geocode")), 
              funs(stringr::str_replace(., "geocode", aggname)))
  
  ## Group by column(s) ending with "_{aggname}" as passed
  geoid_cols <- stringr::str_extract(names(lehdr_df), glue::glue(".*_{aggname}$")) %>% na.omit()
  group_cols <- c("year", "state", geoid_cols)
  group_syms <- rlang::syms(group_cols)
  lehdr_df <- lehdr_df %>%
    group_by(!!!group_syms) %>% 
    summarise_if(is.numeric, funs(sum)) %>% 
    ungroup()
  return(lehdr_df)
}

## quiets concerns of R CMD check re: non-standard evaluation via tidyverse
## per suggestion of Hadley Wickham (https://stackoverflow.com/a/12429344/688693)
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))