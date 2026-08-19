#' Compute earnings tier shares from LODES RAC or WAC data
#'
#' @param lodes_df A data frame (tibble) of LODES RAC or WAC data returned
#'   by [grab_lodes()] with `segment = "S000"` (the total count segment,
#'   which includes all three earnings columns). Must contain columns
#'   `CE01`, `CE02`, and `CE03`. Both WAC and RAC files include these
#'   earnings tier columns; the `type` argument controls only which
#'   geography prefix is used for auto-detection.
#' @param type One of `"wac"` (workplace area characteristics, default) or
#'   `"rac"` (residential area characteristics). Controls the expected
#'   geography column prefix (`w_` for WAC, `h_` for RAC) when
#'   `geo_col = NULL`. Does not affect which earnings columns are used,
#'   as both file types share the `CE01`/`CE02`/`CE03` schema.
#' @param geo_col The name of the geography column to group by, e.g.
#'   `"w_tract"` or `"h_county"`. Defaults to `NULL`, which auto-detects
#'   the first geography column in `lodes_df`.
#' @param output One of `"wide"` (default) or `"long"`. Wide format appends
#'   three share columns. Long format returns one row per geography-tier
#'   combination.
#'
#' @return A tibble with earnings tier counts and shares. In `"wide"` format,
#'   columns are added for `share_low`, `share_mid`, and `share_high`. In
#'   `"long"` format, columns are `tier`, `label`, `count`, and `share`.
#'
#' @description Computes the share of jobs (or workers) in each of the three
#'   LODES monthly earnings tiers:
#'   \describe{
#'     \item{Low (`CE01`)}{Earnings up to $1,250/month.}
#'     \item{Mid (`CE02`)}{Earnings $1,251-$3,333/month.}
#'     \item{High (`CE03`)}{Earnings above $3,333/month.}
#'   }
#'
#'   Earnings shares are useful for tracking wage polarization, identifying
#'   low-wage job concentration, and examining how the earnings structure of
#'   a labor market has shifted over time, especially when combined with
#'   [compute_lodes_change()].
#'
#'   The total denominator is the sum of the three tiers, ensuring shares
#'   sum to 1 within rounding error.
#'
#' @importFrom dplyr group_by summarise mutate select rename all_of any_of across where
#' @importFrom stats setNames
#' @importFrom glue glue
#'
#' @examples
#' \donttest{
#'   wac <- grab_lodes(
#'     state = "md", year = 2019,
#'     lodes_type = "wac", job_type = "JT00",
#'     segment = "S000", agg_geo = "county"
#'   )
#'   compute_earnings_share(wac, type = "wac", geo_col = "w_county")
#'
#'   # Long format, suitable for ggplot2
#'   compute_earnings_share(
#'     wac, type = "wac", geo_col = "w_county", output = "long"
#'   )
#' }
#' @export
compute_earnings_share <- function(
  lodes_df,
  type    = c("wac", "rac"),
  geo_col = NULL,
  output  = c("wide", "long")
) {
  type   <- match.arg(type)
  output <- match.arg(output)

  # Both WAC and RAC use CE01/CE02/CE03 for the three earnings tiers.
  # The `type` argument only affects auto-detection of the geography column
  # prefix (w_ for WAC, h_ for RAC).
  earnings_cols <- c("CE01", "CE02", "CE03")

  missing_cols <- setdiff(earnings_cols, names(lodes_df))
  if (length(missing_cols) > 0) {
    rlang::abort(c(
      glue::glue(
        "Earnings columns not found: {paste(missing_cols, collapse = ', ')}."
      ),
      "i" = glue::glue(
        "Retrieve data with `grab_lodes(..., lodes_type = \"{type}\", ",
        "segment = \"S000\")` to include earnings tier columns."
      )
    ))
  }

  # Auto-detect geography column
  if (is.null(geo_col)) {
    geo_col <- .detect_geo_col(lodes_df)
    rlang::inform(glue::glue("Using `{geo_col}` as the geography column."))
  }
  if (!geo_col %in% names(lodes_df)) {
    rlang::abort(glue::glue("Column `{geo_col}` not found in `lodes_df`."))
  }

  grp_extras <- intersect(c("year", "state"), names(lodes_df))
  grp_cols   <- c(geo_col, grp_extras)

  result <- lodes_df %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(grp_cols))) %>%
    dplyr::summarise(
      count_low  = sum(.data[["CE01"]], na.rm = TRUE),
      count_mid  = sum(.data[["CE02"]], na.rm = TRUE),
      count_high = sum(.data[["CE03"]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      total_earnings = .data$count_low + .data$count_mid + .data$count_high,
      share_low  = dplyr::if_else(
        .data$total_earnings > 0,
        .data$count_low  / .data$total_earnings,
        NA_real_
      ),
      share_mid  = dplyr::if_else(
        .data$total_earnings > 0,
        .data$count_mid  / .data$total_earnings,
        NA_real_
      ),
      share_high = dplyr::if_else(
        .data$total_earnings > 0,
        .data$count_high / .data$total_earnings,
        NA_real_
      )
    )

  if (output == "wide") {
    return(result)
  }

  # Long format: one row per geography-tier combination.
  # Build each tier's data frame without dplyr::across() outside a verb.
  tier_labels <- c(
    low  = "CE01: up to $1,250/month",
    mid  = "CE02: $1,251-$3,333/month",
    high = "CE03: above $3,333/month"
  )

  long_rows <- lapply(c("low", "mid", "high"), function(t) {
    base <- list(
      tier  = t,
      label = tier_labels[[t]],
      count = result[[paste0("count_", t)]],
      share = result[[paste0("share_", t)]]
    )
    # Prepend geography and any group columns by name to preserve column order
    geo_list <- setNames(list(result[[geo_col]]), geo_col)
    extra_list <- lapply(
      setNames(grp_extras, grp_extras),
      function(col) result[[col]]
    )
    dplyr::as_tibble(c(geo_list, extra_list, base))
  })

  dplyr::bind_rows(long_rows)
}
