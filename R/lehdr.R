#' 
#' Download and load LODES data into a data frame (tibble)
#'@param state US state abbreviation in lower case
#'@param  year year of the lodes data
#'@param  lodes_type table type: "origin-destination", "residential association", 
#'or "workplace association". OD files give a home and destination census block
#'for workers. Residential association files give job totals of worker home census 
#'blocks and workplace association files give job totals of worker job census blocks
#'@param job_type Jobtype: "JT00" for all jobs, "JT01" for Primary Jobs, "JT02" for All Private Jobs,
#' "JT03" for Private Primary Jobs, "JT04" for All Federal Jobs, "JT05" for Federal Primary Jobs
#'@param segment Segment of the workforce: "S000" total number of jobs for workers,
#' "SA01" number of jobs forworker aged 29 or younger, "SA02" number of jobs for workers 
#' aged 30-54,"SA03" number of jobs for workers 55 and older, "SE01" number of jobs 
#' with earnings $1,250/month or less, "SE02" number of jobs with earnings $1,251 to 
#' $3,333/month, "SE03" number of jobs with earnings greater than $3,333/month, 
#' "SI01" number of jobs in Goods Producing industry sectors, "SI02" number of jobs in
#' Trade, Transportation, and Utilities industry sectors, "SI03" number of jobs in All Other Services 
#' industry sectors 
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
#' 
#' 
#' @export
#
#
#


grab_lodes <- function(state, year, lodes_type = c("od", "rac", "wac"), 
                       job_type = c("JT00", "JT01", "JT02", "JT03", "JT04", "JT05"), 
                       segment = c("S000", "SA01", "SA02", "SA03", "SE01", "SE02",
                                   "SE03", "SI01", "SI02", "SI03"),  
                       tract = FALSE, state_part = NULL, download_dir = getwd()) {
  
  state <- tolower(state)
  type <- match.arg(tolower(lodes_type), c("od", "rac", "wac"))
  
  url <- if(type == "od") {
    url <- 
      glue::glue("https://lehd.ces.census.gov/data/lodes/LODES7/{state}/{lodes_type}/{state}_{lodes_type}_{state_part}_{job_type}_{year}.csv.gz")
  } else{
    url <- 
      glue::glue("https://lehd.ces.census.gov/data/lodes/LODES7/{state}/{lodes_type}/{state}_{lodes_type}_{segment}_{job_type}_{year}.csv.gz")
  }
  
  httr::stop_for_status(httr::HEAD(url),
                        "data for this combination of features was not found on LODES")
  
  fil <- file.path(path.expand(download_dir), basename(url))
  
  if(file.exists(fil)) {
    message(glue::glue("Cached version of file found in {fil}\n Reading now..."))
  } else {
    message(glue::glue("Downloading {url} to {fil} now..."))
    res <- httr::GET(url, httr::write_disk(fil))
  }
  
  if (lodes_type != "od") {
    
    df <- suppressMessages(readr::read_csv(fil, 
                                         col_types = cols(w_geocode = col_character(), 
                                                        createdate = col_character())))
  } else {
    
    df <- suppressMessages(readr::read_csv(fil, 
                                           col_types = cols(w_geocode = col_character(),
                                                            h_geocode = col_character(),
                                                            createdate = col_character())))
  }
  
  df <- df %>% 
    mutate(year=year,
           state=toupper(state))
  
  
  if (tract == TRUE & lodes_type == "od") {
    
    df$w_tract_id <-  stringr::str_sub(df$w_geocode, 1, 11)
    df$h_tract_id <- stringr::str_sub(df$h_geocode, 1, 11)
    df <- df %>% select(-w_geocode, -h_geocode, -createdate)
    df <- df %>% 
      group_by(w_tract_id, h_tract_id, year) %>% 
      summarise_if(is.numeric, funs(sum)) %>% 
      mutate(State = toupper(state))
    
  } else if(tract == TRUE) {
    
    df$w_tract_id <-  stringr::str_sub(df$w_geocode, 1, 11)
    df <- df %>% select(-w_geocode, -createdate)
    df <- df %>% 
      group_by(w_tract_id, year) %>% 
      summarise_if(is.numeric, funs(sum))  %>% 
      mutate(State = toupper(state))
    }
  
  return(df)
  
}