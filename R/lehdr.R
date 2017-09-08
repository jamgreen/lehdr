#------------------------------------------------------------
#' Download and load LEHD WAC data into a data frame (tibble)
#'
#' \code{grab_wac} downloads and loads LEHD WAC data into a data frame (tibble)
#'
#' @param state US state abbrevation in lower case.
#' @param year year of the wac data.
#' @param segment Segment of the workforce: "S000" total number of jobs for workers,
#' "SA01" number of jobs forworker aged 29 or younger, "SA02" number of jobs for workers 
#' aged 30-54,"SA03" number of jobs for workers 55 and older, "SE01" number of jobs 
#' with earnings $1,250/month or less, "SE02" number of jobs with earnings $1,251 to 
#' $3,333/month, "SE03" number of jobs with earnings greater than $3,333/month, 
#' "SI01" number of jobs in Goods Producing industry sectors, "SI02" number of jobs in
#' Trade, Transportation, and Utilities industry sectors, "SI03" number of jobs in All Other Services 
#' industry sectors 
#' @param jobtype Jobtype: "JT00" for all jobs, "JT01" for Primary Jobs, "JT02" for All Private Jobs,
#' "JT03" for Private Primary Jobs, "JT04" for All Federal Jobs, "JT05" for Federal Primary Jobs
#' @param tract logical whether to aggregate w_geocode to w_tract_id.
#' @return A data frame containing block or tract level WAC.
#' @import dplyr
#' @importFrom readr read_csv cols col_character
#' @export
#'
grab_wac <-  function(state, year=2014, segment = "S000", jobtype = "JT00", tract = FALSE) {
    url <- glue::glue("https://lehd.ces.census.gov/data/lodes/LODES7/{state}/wac/{state}_wac_{segment}_{jobtype}_{year}.csv.gz")

    df <-  read_csv(url, col_types = cols(w_geocode = col_character(), 
                                          createdate = col_character()))
    df <- df %>% 
      mutate(year=year,
             state=toupper(state))

    if (tract == TRUE) {
      df$w_tract_id <-  stringr::str_sub(df$w_geocode, 1, 11)
      df <- df %>% select(-w_geocode, -createdate)
      df <- df %>% 
        group_by(state, w_tract_id) %>% 
        summarise_if(is.numeric, funs(sum))
    }

    return(df)
}

#------------------------------------------------------------
#' Download and load LEHD RAC data into a data frame (tibble)
#' 
#' TODO
#' @param state US state abbrevation in lower case.
#' @param year year of the wac data.
#' @param segment Segment of the workforce: "S000" total number of jobs for workers,
#' "SA01" number of jobs forworker aged 29 or younger, "SA02" number of jobs for workers 
#' aged 30-54,"SA03" number of jobs for workers 55 and older, "SE01" number of jobs 
#' with earnings $1,250/month or less, "SE02" number of jobs with earnings $1,251 to 
#' $3,333/month, "SE03" number of jobs with earnings greater than $3,333/month, 
#' "SI01" number of jobs in Goods Producing industry sectors, "SI02" number of jobs in
#' Trade, Transportation, and Utilities industry sectors, "SI03" number of jobs in All Other Services 
#' industry sectors 
#' @param jobtype Jobtype: "JT00" for all jobs, "JT01" for Primary Jobs, "JT02" for All Private Jobs,
#' "JT03" for Private Primary Jobs, "JT04" for All Federal Jobs, "JT05" for Federal Primary Jobs
#' @param tract logical whether to aggregate h_geocode to h_tract_id.
#' @return A data frame containing block or tract level WAC.
#' @import dplyr
#' @importFrom readr read_csv cols col_character
#' @export
grab_rac <- function(state, year = 2014, segment = "S000", jobtype = "JT00", tract = FALSE) {
    url <- glue::glue("https://lehd.ces.census.gov/data/lodes/LODES7/{state}/rac/{state}_rac_{segment}_{jobtype}_{year}.csv.gz")
    df <- read_csv(url, col_types = cols(h_geocode = col_character(), 
                                         createdate = col_character()))
    df <- df %>% 
      mutate(year=year,
             state=toupper(state))

    if (tract == TRUE){
      df$h_tract_id <- stringr::str_sub(df$h_geocode, 1, 11)
      df <- df %>% select(-h_geocode, -createdate)
      df <- df %>% 
        group_by(state, h_tract_id) %>% 
        summarise_if(is.numeric, funs(sum))
    }
    
    return(df)
  }

#-----------------------------------------------------------
#' Download and load LEHD OD data into a data frame (tibble)
#' 
#' TODO
#' @param state US state abbrevation in lower case.
#' @param year year of the wac data.
#' @param main Part of state file: "main" includes jobs with both workplace and residence in the state,
#' "aux" includes jobs with workplace in the state and residence out of state
#' @param jobtype Jobtype: "JT00" for all jobs, "JT01" for Primary Jobs, "JT02" for All Private Jobs,
#' "JT03" for Private Primary Jobs, "JT04" for All Federal Jobs, "JT05" for Federal Primary Jobs
#' @param tract logical whether to aggregate h_geocode to h_tract_id and w_geocode to w_tract_id.
#' @return A data frame containing block or tract level OD.
#' @import dplyr
#' @importFrom readr read_csv cols col_character
#' @export
grab_od <- function(state, year = 2014, main = "main", jobtype = "JT00", tract = FALSE) {

    url <- glue::glue("https://lehd.ces.census.gov/data/lodes/LODES7/{state}/od/{state}_od_{main}_{jobtype}_{year}.csv.gz")
    df <- read_csv(url, col_types = cols(w_geocode = col_character(), 
                                         h_geocode = col_character(),
                                         createdate = col_character()))
    df$year <- year

    if (tract == TRUE){
      df$w_tract_id <- stringr::str_sub(df$w_geocode, 1, 11)
      df$h_tract_id <- stringr::str_sub(df$h_geocode, 1, 11)
      df <- df %>% select(-w_geocode, -h_geocode, -createdate)
      df <- df %>% 
        group_by(w_tract_id, h_tract_id) %>% 
        summarise_if(is.numeric, funs(sum))
    }
    return(df)
  }
