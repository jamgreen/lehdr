#' Compute longitudinal change in LODES data across years
#'
#' @param lodes_df A data frame (tibble) of LODES data covering at least two
#'   years, as returned by [grab_lodes()] with a vector of years. Must
#'   contain a `year` column and one or more numeric columns to difference.
#' @param geo_col The name of the geography column to group by, e.g.
#'   `"w_tract"`, `"h_county"`, or `"w_geocode"`. Defaults to `NULL`, which
#'   auto-detects the first column ending in `_geocode`, `_tract`, `_county`,
#'   `_bg`, or `_state`.
#' @param base_year The reference year for computing change. Defaults to the
#'   earliest year present in `lodes_df`.
#' @param compare_year The target year for computing change. Defaults to the
#'   latest year present in `lodes_df`.
#' @param variables Optional character vector of numeric column names to
#'   include in the output. Defaults to all numeric columns (excluding `year`).
#' @param output One of `"wide"` (default) or `"long"`. In wide format,
#'   absolute and percentage change columns are appended for each variable. In
#'   long format, each variable is a row with columns `variable`,
#'   `base_value`, `compare_value`, `change`, and `pct_change`.
#'
#' @return A tibble of change statistics. In `"wide"` format, columns follow
#'   the pattern `{variable}_base`, `{variable}_compare`,
#'   `{variable}_change`, and `{variable}_pct_change`. In `"long"` format,
#'   columns are `variable`, `base_value`, `compare_value`, `change`, and
#'   `pct_change`.
#'
#' @description Computes absolute and percentage change in LODES variables
#'   between two years for each geographic unit. This is particularly useful
#'   for tracking shifts in employment, industrial composition, or earnings
#'   structure over time.
#'
#'   Percentage change is computed as
#'   \eqn{(\text{compare} - \text{base}) / \text{base} \times 100}.
#'   Returns `NA` where `base_value` is zero or missing.
#'
#' @importFrom dplyr filter select group_by summarise left_join mutate rename all_of any_of where
#' @importFrom rlang :=
#' @importFrom tidyr pivot_longer
#' @importFrom glue glue
#'
#' @examples
#' \donttest{
#'   wac_multi <- grab_lodes(
#'     state = "md", year = c(2015, 2019),
#'     lodes_type = "wac", job_type = "JT00",
#'     segment = "S000", agg_geo = "county"
#'   )
#'   compute_lodes_change(wac_multi, geo_col = "w_county")
#'
#'   # Long format
#'   compute_lodes_change(
#'     wac_multi,
#'     geo_col  = "w_county",
#'     output   = "long",
#'     variables = c("C000", "CE01", "CE02", "CE03")
#'   )
#' }
#' @export
compute_lodes_change <- function(
  lodes_df,
  geo_col      = NULL,
  base_year    = NULL,
  compare_year = NULL,
  variables    = NULL,
  output       = c("wide", "long")
) {
  output <- match.arg(output)

  if (!"year" %in% names(lodes_df)) {
    rlang::abort(c(
      "`lodes_df` must contain a `year` column.",
      "i" = "Retrieve data with `grab_lodes()` using a vector of years."
    ))
  }

  years_present <- sort(unique(lodes_df$year))
  if (length(years_present) < 2) {
    rlang::abort(c(
      "`lodes_df` must contain data for at least two distinct years.",
      "i" = glue::glue("Only year {years_present} found.")
    ))
  }

  if (is.null(base_year))    base_year    <- min(years_present)
  if (is.null(compare_year)) compare_year <- max(years_present)

  if (!base_year %in% years_present) {
    rlang::abort(glue::glue(
      "`base_year` {base_year} not found in `lodes_df`. ",
      "Available years: {paste(years_present, collapse = ', ')}."
    ))
  }
  if (!compare_year %in% years_present) {
    rlang::abort(glue::glue(
      "`compare_year` {compare_year} not found in `lodes_df`. ",
      "Available years: {paste(years_present, collapse = ', ')}."
    ))
  }

  # Auto-detect geo column if not supplied
  if (is.null(geo_col)) {
    geo_col <- .detect_geo_col(lodes_df)
    rlang::inform(glue::glue("Using `{geo_col}` as the geography column."))
  }
  if (!geo_col %in% names(lodes_df)) {
    rlang::abort(glue::glue("Column `{geo_col}` not found in `lodes_df`."))
  }

  # Determine numeric variables to compare
  if (is.null(variables)) {
    variables <- names(dplyr::select(lodes_df, where(is.numeric), -dplyr::all_of("year")))
  }
  missing_vars <- setdiff(variables, names(lodes_df))
  if (length(missing_vars) > 0) {
    rlang::abort(glue::glue(
      "Variable(s) not found in `lodes_df`: {paste(missing_vars, collapse = ', ')}."
    ))
  }

  grp_extras <- intersect(c("state"), names(lodes_df))
  grp_cols   <- c(geo_col, grp_extras)

  base_df <- lodes_df %>%
    dplyr::filter(.data$year == base_year) %>%
    dplyr::select(dplyr::all_of(c(grp_cols, variables)))

  compare_df <- lodes_df %>%
    dplyr::filter(.data$year == compare_year) %>%
    dplyr::select(dplyr::all_of(c(grp_cols, variables)))

  combined <- dplyr::left_join(
    base_df,
    compare_df,
    by    = grp_cols,
    suffix = c("_base", "_compare")
  )

  for (v in variables) {
    b <- glue::glue("{v}_base")
    c_ <- glue::glue("{v}_compare")
    combined[[glue::glue("{v}_change")]] <-
      combined[[c_]] - combined[[b]]
    combined[[glue::glue("{v}_pct_change")]] <- dplyr::if_else(
      !is.na(combined[[b]]) & combined[[b]] != 0,
      (combined[[c_]] - combined[[b]]) / combined[[b]] * 100,
      NA_real_
    )
  }

  if (output == "wide") {
    return(combined)
  }

  # Long format
  long_rows <- lapply(variables, function(v) {
    dplyr::tibble(
      !!geo_col        := combined[[geo_col]],
      variable          = v,
      base_year         = base_year,
      compare_year      = compare_year,
      base_value        = combined[[glue::glue("{v}_base")]],
      compare_value     = combined[[glue::glue("{v}_compare")]],
      change            = combined[[glue::glue("{v}_change")]],
      pct_change        = combined[[glue::glue("{v}_pct_change")]]
    )
  })

  dplyr::bind_rows(long_rows)
}


# Auto-detect the primary geography column
# @noRd
.detect_geo_col <- function(df) {
  patterns <- c("_geocode$", "_tract$", "_county$", "_bg$", "_state$")
  for (p in patterns) {
    candidates <- grep(p, names(df), value = TRUE)
    if (length(candidates) > 0) return(candidates[1])
  }
  rlang::abort(c(
    "Could not auto-detect a geography column in `lodes_df`.",
    "i" = "Supply the column name explicitly via the `geo_col` argument."
  ))
}
