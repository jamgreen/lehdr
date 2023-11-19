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
#' \donttest{
#' # Download and load current geographic crosswalk for Alaska
#' alaska_xwalk <- grab_crosswalk('AK')
#' 
#' # Download and load current geographic crosswalk for New England#' 
#' new_england_xwalk <- grab_crosswalk(c('CT', 'RI', 'MA', 'VT', 'NH', 'ME'))
#' 
#' }

grab_crosswalk <- function(state, 
                           download_dir = file.path(tools::R_user_dir("lehdr",
                                                                      which="cache"))){
  
  states <- tolower(state)
  
  urls <- glue::glue("https://lehd.ces.census.gov/data/lodes/LODES8/{states}/{states}_xwalk.csv.gz")
  
  for (url in urls){
    httr::stop_for_status(httr::HEAD(url),
                          paste0("Data for this state was not found on LODES.\nURL: ", url))
  }
  
  vdownload_xwalk(url = urls, download_dir = download_dir) %>%
    vread_xwalk() %>%
    dplyr::bind_rows()
  
}

download_xwalk <- function(url, download_dir){
  # Set download directory, check for cache
  download_dir <- path.expand(download_dir)
  if (!dir.exists(download_dir))
    dir.create(download_dir, recursive=TRUE)
  fil <- normalizePath(file.path(download_dir, basename(url)), mustWork = FALSE)
  
  # Read from FTP site
  if (file.exists(fil)) {
    message(glue::glue("Cached version of file found in {fil}\n"))
  } else {
    message(glue::glue("Downloading {url} to {fil}"))
    res <- httr::GET(url, httr::write_disk(fil))
    message(glue::glue("Download complete."))
  }
  
  return(fil)
  
}

read_xwalk <- function(filepath){
  res <- suppressMessages(readr::read_csv(filepath, col_types = readr::cols(.default = 'c')))
  download_dir <- dirname(filepath)
  
  # Remove cached files now that they're read in
  # Unlink returns 0 for success, 1 for failure 
  if(unlink(filepath)) {
    message(glue::glue("Could not clear crosswalk cache."))
  } else {
    message(glue::glue("Crosswalk cache cleared."))
    # Now check to see if the cache directory is empty, remove it if it is
    if(length(list.files(download_dir)) == 0) {
      unlink(download_dir, recursive = TRUE)
    }
    
  }
  
  return(res)
}

vdownload_xwalk <- Vectorize(download_xwalk, vectorize.args = 'url')
vread_xwalk <- Vectorize(read_xwalk, vectorize.args = 'filepath', SIMPLIFY = FALSE)