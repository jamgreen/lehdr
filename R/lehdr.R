#------------------------------------------------------------
#' Download and load LEHD WAC data into a data frame (tibble)
#'
#' \code{grab_wac} downloads and loads LEHD WAC data into a data frame (tibble)
#'
#' @param state US state abbrevation in lower case.
#' @param year year of the wac data.
#' @param segment ...
#' @param jobtype ...
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
#' @param segment TODO
#' @param jobtype TODO
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
#' @param main TODO
#' @param jobtype TODO
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
