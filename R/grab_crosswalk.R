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
#' @import httr2
#' @importFrom glue glue
#' @importFrom dplyr bind_rows `%>%`
#' @importFrom readr read_csv cols col_character
#' 
#' @examples
#' \donttest{
#' # Download and load current geographic crosswalk for Alaska
#' alaska_xwalk <- grab_crosswalk("VT")
#' 
#' # Download and load current geographic crosswalk for small states
#' small_states_xwalk <- grab_crosswalk(c("wy", 'ND', 'SD'))
#' }
#' 

grab_crosswalk <- function(state, 
                           download_dir = normalizePath(file.path(tools::R_user_dir("lehdr",
                                                                      which="cache")),
                                                        mustWork = FALSE)) {
  
  states <- tolower(state)
  
  urls <- glue::glue("https://lehd.ces.census.gov/data/lodes/LODES8/{states}/{states}_xwalk.csv.gz")
  
  # Old method from httr - could be converted to httr2, but error handling is in
  # the request now (see download_xwalk).
  # for (url in urls){
  #   httr::stop_for_status(httr::HEAD(url),
  #                         paste0("Data for this state was not found on LODES.\nURL: ", url))
  # }
  
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
  
  # Create httr2 request from url
  lodes_req <- request(url)

  # Perform request and handle connection errors, writing response to disk
  rlang::inform(glue::glue("Downloading crosswalk {url} to {fil}"))
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

  return(fil)
}

read_xwalk <- function(filepath){
  res <- suppressMessages(readr::read_csv(filepath, col_types = readr::cols(.default = 'c')))
  download_dir <- dirname(filepath)
  filename <- basename(filepath)
  
  # Remove cached files now that they're read in
  # Unlink returns 0 for success, 1 for failure
  if(unlink(filepath)) {
    rlang::inform(glue::glue("Could not clear {filename} from crosswalk cache."))
  } else {
    
    rlang::inform(glue::glue("{filename} crosswalk cleared from cache."))
    # Now check to see if the cache directory is empty, remove it if it is
    if(length(list.files(download_dir)) == 0) {
      unlink(download_dir, recursive = TRUE)
    }
    
  }
  
  return(res)
}

vdownload_xwalk <- Vectorize(download_xwalk, vectorize.args = 'url')
vread_xwalk <- Vectorize(read_xwalk, vectorize.args = 'filepath', SIMPLIFY = FALSE)