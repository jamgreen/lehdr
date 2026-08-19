#' Download and load LODES geographic crosswalk into a data frame (tibble)
#'
#' @param state US state abbreviation in lower case. Can be a vector of
#'   state abbreviations, e.g. `c("wy", "nd", "sd")`.
#' @param version The LODES version whose crosswalk to download. Must be
#'   one of `"LODES8"` (default), `"LODES7"`, or `"LODES5"`. The crosswalk
#'   maps Census blocks to higher-level geographies for that vintage.
#' @param download_dir Directory where the crosswalk file will be downloaded.
#'   Defaults to the user-level cache directory for `lehdr`.
#'
#' @return A tibble containing the geographic crosswalk at the Census block
#'   level, with columns linking blocks to block groups, tracts, counties,
#'   and states.
#'
#' @description Download the LODES geographic crosswalk for one or more
#'   states. The crosswalk maps Census block GEOIDs to higher-level
#'   geographies and is useful for custom aggregations outside the built-in
#'   `agg_geo` argument in [grab_lodes()].
#'
#' @import httr2
#' @importFrom glue glue
#' @importFrom dplyr bind_rows `%>%`
#' @importFrom readr read_csv cols col_character
#'
#' @examples
#' \donttest{
#'   # Download crosswalk for Vermont
#'   vt_xwalk <- grab_crosswalk("vt")
#'
#'   # Download crosswalk for several small states
#'   small_xwalk <- grab_crosswalk(c("wy", "nd", "sd"))
#'
#'   # Download a LODES7 crosswalk (2010 Census block vintage)
#'   vt_xwalk_7 <- grab_crosswalk("vt", version = "LODES7")
#' }
#' @export
grab_crosswalk <- function(
  state,
  version      = c("LODES8", "LODES7", "LODES5"),
  download_dir = normalizePath(
    file.path(tools::R_user_dir("lehdr", which = "cache")),
    mustWork = FALSE
  )
) {
  version <- toupper(rlang::arg_match(version))
  states  <- tolower(trimws(as.character(state)))

  urls <- glue::glue(
    "https://lehd.ces.census.gov/data/lodes/{version}/{states}/{states}_xwalk.csv.gz"
  )

  vdownload_xwalk(url = urls, download_dir = download_dir) %>%
    vread_xwalk() %>%
    dplyr::bind_rows()
}


# ----------------------------------------------------------------------
# Internal: download a single crosswalk file
# @noRd
# ----------------------------------------------------------------------
download_xwalk <- function(url, download_dir) {
  download_dir <- path.expand(download_dir)
  if (!dir.exists(download_dir)) {
    dir.create(download_dir, recursive = TRUE)
  }
  lodes_version <- tolower(regmatches(url, regexpr("LODES[0-9]+", url)))
  fil <- normalizePath(
    file.path(download_dir, paste0(lodes_version, "_", basename(url))),
    mustWork = FALSE
  )

  rlang::inform(glue::glue("Downloading crosswalk {url} to {basename(fil)}"))

  lodes_req  <- request(url)
  lodes_resp <- withCallingHandlers(
    lodes_req |>
      req_error(is_error = \(r) FALSE) |>
      req_perform(path = fil),
    httr2_failure = function(cnd) {
      rlang::abort(c(
        "lehdr: Could not connect to the LODES server.",
        "i" = "Please check your internet connection."
      ))
    }
  )

  if (lodes_resp$status_code >= 400) {
    rlang::abort(c(
      glue::glue("Server error downloading crosswalk: {url}"),
      "i" = glue::glue("HTTP {lodes_resp$status_code} returned."),
      "i" = "https://lehd.ces.census.gov/data/lodes/LODES8/"
    ))
  } else if (length(lodes_resp$body) < 1) {
    rlang::abort(c(
      glue::glue("Empty response downloading crosswalk: {url}"),
      "i" = "Check your internet connection."
    ))
  } else {
    rlang::inform(glue::glue("Download complete for {basename(fil)}"))
  }

  return(fil)
}


# ----------------------------------------------------------------------
# Internal: read and clean up a crosswalk file
# @noRd
# ----------------------------------------------------------------------
read_xwalk <- function(filepath) {
  res <- suppressMessages(readr::read_csv(
    filepath,
    col_types = readr::cols(.default = "c")
  ))
  download_dir <- dirname(filepath)
  filename     <- basename(filepath)

  if (unlink(filepath) != 0L) {
    rlang::inform(glue::glue("Could not remove {filename} from crosswalk cache."))
  } else {
    rlang::inform(glue::glue("{filename} crosswalk cleared from cache."))
    if (length(list.files(download_dir)) == 0L) {
      unlink(download_dir, recursive = TRUE)
    }
  }

  return(res)
}

vdownload_xwalk <- Vectorize(download_xwalk, vectorize.args = "url")
vread_xwalk     <- Vectorize(read_xwalk, vectorize.args = "filepath", SIMPLIFY = FALSE)
