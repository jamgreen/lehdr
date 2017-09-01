if(!require(pacman)){install.packages("pacman");library(pacman)}
p_load(readr, stringr, sf, glue,tidycensus, tigris, dplyr)
options(tigris_class = "sf", tigris_use_cache = TRUE)


  
  grab_wac <-  function(state, year=2014, segment = "S000", jobtype = "JT00", tract = FALSE) {
    require(readr)
    require(stringr)
    require(dplyr)
    require(glue)
    

    url <- glue("https://lehd.ces.census.gov/data/lodes/LODES7/{state}/wac/{state}_wac_{segment}_{jobtype}_{year}.csv.gz")
    
    df <-  read_csv(url, col_types = cols(w_geocode = col_character(), createdate = col_character()))
      
    if (tract == TRUE) {
      df$tract_id <-  str_sub(df$w_geocode, 1, 11)
      df <- df %>% select(-w_geocode, -createdate)
      df <- df %>% group_by(tract_id) %>% summarise_all(sum)
    }

    return(df)
  }
  
  grab_rac <- function(state, year = 2014, segment = "S000", jobtype = "JT00", tract = FALSE) {
    require(readr)
    require(stringr)
    require(dplyr)
    require(glue)
    
    url <- glue("https://lehd.ces.census.gov/data/lodes/LODES7/{state}/rac/{state}_rac_{segment}_{jobtype}_{year}.csv.gz")
    df <- read_csv(url, col_types = cols(h_geocode = col_character(), createdate = col_character()))
    
    if (tract == TRUE){
      df$tract_id <- str_sub(df$h_geocode, 1, 11)
      df <- df %>% select(-h_geocode, -createdate)
      df <- df %>% group_by(tract_id) %>% summarise_all(sum)
    }
    return(df)
  }
  
  grab_od <- function(state, year = 2014, main = "main", jobtype = "JT00" ,tract = FALSE) {
    require(readr)
    require(stringr)
    require(dplyr)
    require(glue)
    
    url <- glue("https://lehd.ces.census.gov/data/lodes/LODES7/{state}/od/{state}_od_{main}_{jobtype}_{year}.csv.gz")
    df <- read_csv(url, col_types = cols(w_geocode = col_character(),h_geocode = col_character(), 
                                         createdate = col_character()))
    
    if (tract == TRUE){
      df$work_tract_id <- str_sub(df$w_geocode, 1, 11)
      df$home_tract_id <- str_sub(df$h_geocode, 1, 11)
      df <- df %>% select(-w_geocode, -h_geocode, -createdate)
      df <- df %>% group_by(work_tract_id, home_tract_id) %>% summarise_all(sum)
    }
    return(df)
  }
  
  
  