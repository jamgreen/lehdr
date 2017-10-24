#' 
#' Download and load LODES data into a data frame (tibble)
#' 
#' @param state US state abbreviation in lower case, can be a vector of states
#' @param year year of the lodes data, can be a vector of years
#' @param lodes_type table type: "origin-destination", "residential association", 
#'        or "workplace association". OD files give a home and destination census block
#'        for workers. Residential association files give job totals of worker home census 
#'        blocks and workplace association files give job totals of worker job census blocks
#' @param job_type Jobtype: "JT00" for all jobs, "JT01" for Primary Jobs, "JT02" for All Private Jobs,
#'        "JT03" for Private Primary Jobs, "JT04" for All Federal Jobs, "JT05" for Federal Primary Jobs
#' @param segment Segment of the workforce: "S000" total number of jobs for
#'   workers, "SA01" number of jobs forworker aged 29 or younger, "SA02" number
#'   of jobs for workers aged 30-54,"SA03" number of jobs for workers 55 and
#'   older, "SE01" number of jobs with earnings $1,250/month or less, "SE02"
#'   number of jobs with earnings $1,251 to $3,333/month, "SE03" number of jobs
#'   with earnings greater than $3,333/month, "SI01" number of jobs in Goods
#'   Producing industry sectors, "SI02" number of jobs in Trade, Transportation,
#'   and Utilities industry sectors, "SI03" number of jobs in All Other Services
#'   industry sectors
#' @param state_part Part of the state file, can have values of "main" or "aux" in OD files. "Main"
#' includes workers with their workplace and residence in the state. "Aux" includes workers with 
#' residences out of state and workplace in the state of interest
#' @param tract logical whether to aggregate w_geocode to w_tract_id.
#' @param download_dir Directory where lodes table will be downloaded
#' 
#' @description Download LODES OD, RAC, and WAC tables
#' @return a dataframe (tibble) of block or tract level LODES files
#' @import dplyr
#' @importFrom readr read_csv cols col_character 
#' @importFrom httr GET stop_for_status HEAD write_disk
#' @importFrom glue glue
#' @importFrom stats na.omit
#'  
#' @export

grab_lodes <- function(state, year, lodes_type = c("od", "rac", "wac"), 
                       job_type = c("JT00", "JT01", "JT02", "JT03", "JT04", "JT05"), 
                       segment = c("S000", "SA01", "SA02", "SA03", "SE01", "SE02",
                                   "SE03", "SI01", "SI02", "SI03"),  
                       tract = FALSE, state_part = NULL, 
                       download_dir = file.path(getwd(), "lodes_raw")) {
  
  if (length(state) > 1 | length(year) > 1) {
    ## Handle multiple states x years
    state_year <- expand.grid(state=state, year=year)
    results <- apply(state_year, 1, function(df_row) grab_lodes(df_row[1], df_row[2], 
                                                           lodes_type = lodes_type,
                                                           job_type = job_type,
                                                           segment = segment,
                                                           tract = tract,
                                                           state_part = state_part,
                                                           download_dir = download_dir) 
                     )
    return(dplyr::bind_rows(results))
  }
  
  state <- tolower(state)
  type <- match.arg(tolower(lodes_type), c("od", "rac", "wac"))
  
  url <- ifelse(type == "od", 
                glue::glue("https://lehd.ces.census.gov/data/lodes/LODES7/{state}/{lodes_type}/{state}_{lodes_type}_{state_part}_{job_type}_{year}.csv.gz"),
                glue::glue("https://lehd.ces.census.gov/data/lodes/LODES7/{state}/{lodes_type}/{state}_{lodes_type}_{segment}_{job_type}_{year}.csv.gz")
  )
  
  httr::stop_for_status(httr::HEAD(url),
                        "data for this combination of features was not found on LODES")
  
  download_dir <- path.expand(download_dir)
  if (!dir.exists(download_dir))
    dir.create(download_dir)
  fil <- file.path(download_dir, basename(url))
  
  if(file.exists(fil)) {
    message(glue::glue("Cached version of file found in {fil}\n Reading now..."))
  } else {
    message(glue::glue("Downloading {url} to {fil} now..."))
    res <- httr::GET(url, httr::write_disk(fil))
  }
  
  col_types <- cols(w_geocode = col_character(), 
                    h_geocode = col_character(),
                    createdate = col_character())
  
  if (lodes_type == "rac")
    col_types$cols$w_geocode <- NULL
  if (lodes_type == "wac")
    col_types$cols$h_geocode <- NULL
  
  df <- suppressMessages(readr::read_csv(fil, col_types = col_types))

  df <- df %>% mutate(
    year=year,
    State=toupper(state))
  
  if (tract==TRUE) {
    ## Convert *_geocode column(s) to tract id
    df <- df %>% 
      mutate_at(vars(ends_with("_geocode")), funs(stringr::str_sub(., 1, 11))) %>% 
      rename_at(vars(ends_with("_geocode")), 
                funs(stringr::str_replace(., "geocode", "tract_id")))
    
    ## Group by column(s) ending with "_tract_id"
    tract_cols <- stringr::str_extract(names(df), ".*_tract_id$") %>% na.omit()
    group_cols <- c("year", "State", tract_cols)
    group_syms <- rlang::syms(group_cols)
    df <- df %>% 
      group_by(!!!group_syms) %>% 
      summarise_if(is.numeric, funs(sum))
  }

  return(df)
  
}
