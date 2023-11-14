#' Download and load LODES geographic crosswalk into a data frame (tibble)
#'
#' @param state US state abbreviation in lower case, can be a vector of states.
#' @param download_dir Directory where lodes table will be downloaded.
#'
#' @return a dataframe (tibble) geographic crosswalk at the block level 
#' @export
#'
#' @description Download LODES geographic crosswalk
#' 
#' @importFrom glue glue
#' @importFrom httr GET write_disk HEAD stop_for_status
#' @importFrom dplyr bind_rows `%>%`
#' @importFrom readr read_csv cols col_character
#' 
#' @examples
#' \dontrun{
#' # Download and load current geographic crosswalk for Alaska
#' alaska_xwalk <- grab_crosswalk('AK')
#' 
#' # Download and load current geographic crosswalk for New England
#' 
#' new_england_xwalk <- grab_crosswalk(c('CT', 'RI', 'MA', 'VT', 'NH', 'ME'))
#' 
#' }

grab_crosswalk <- function(state, 
                           download_dir = file.path(tools::R_user_dir("lehdr", which="cache"))){
  
  states <- tolower(state)
  
  urls <- glue::glue("https://lehd.ces.census.gov/data/lodes/LODES7/{states}/{states}_xwalk.csv.gz")
  
  for (url in urls){
    httr::stop_for_status(httr::HEAD(url),
                          paste0("download crosswalk. Data for this state was not found on LODES.\n  URL: ", url))
  }
  
  
  vdownload_xwalk(url = urls, download_dir = download_dir) %>%
    vread_xwalk() %>%
    dplyr::bind_rows()
  
}

download_xwalk <- function(url, download_dir){
  
  fil <- file.path(path.expand(download_dir), basename(url))
  
  if (file.exists(fil)) {
    
    message(glue::glue("Cached version of file found in {fil}\n"))
    
  } else {
    
    message(glue::glue("Downloading {url} to {fil} now..."))
    res <- httr::GET(url, httr::write_disk(fil))
  }
  
  return(fil)
  
}

read_xwalk <- function(filepath){
  
  res <- suppressMessages(readr::read_csv(filepath, col_types = readr::cols(.default = 'c')))
  
  res
}

vdownload_xwalk <- Vectorize(download_xwalk, vectorize.args = 'url')
vread_xwalk <- Vectorize(read_xwalk, vectorize.args = 'filepath', SIMPLIFY = FALSE)