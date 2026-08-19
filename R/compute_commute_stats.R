#' Compute commute flow statistics from LODES OD data
#'
#' @param od_df A data frame (tibble) of LODES origin-destination data
#'   returned by [grab_lodes()] with `lodes_type = "od"`. Must include
#'   columns `h_{agg_geo}` (home geography), `w_{agg_geo}` (work geography),
#'   and `S000` (total job count). The data frame may be at any aggregation
#'   level supported by `agg_geo`.
#'
#'   **Note on row structure:** LODES OD files are a flow matrix. A call to
#'   [grab_lodes()] with `lodes_type = "od"` returns one row per
#'   *origin-destination pair*, not one row per geography -- even after
#'   aggregation via `agg_geo`. For example, a county-level OD pull for West
#'   Virginia returns ~2,800 rows (one per observed county-county flow pair),
#'   not 55 (the number of counties). Pass the result directly to
#'   `compute_commute_stats()` to reduce the pair table to one row per
#'   geography with inflow, outflow, net flow, and self-containment.
#' @param agg_geo The geographic level of the OD data. Must match the level
#'   at which `od_df` was retrieved or aggregated. One of `"block"`, `"bg"`,
#'   `"tract"`, `"county"`, or `"state"`. Defaults to `"tract"`.
#'
#' @return A tibble with one row per geography, containing:
#' \describe{
#'   \item{`{agg_geo}`}{The geographic identifier.}
#'   \item{`year`}{Year of the data (if present in `od_df`).}
#'   \item{`state`}{State FIPS abbreviation (if present in `od_df`).}
#'   \item{`workers_in`}{Total workers arriving (working in this geography).}
#'   \item{`workers_out`}{Total workers departing (living here, working elsewhere).}
#'   \item{`workers_internal`}{Workers whose home and work are both in this geography (internal flows).}
#'   \item{`net_flow`}{Net worker flow: `workers_in - workers_out`. Positive values indicate net job importers.}
#'   \item{`self_containment`}{Share of resident workers who also work in this geography: `workers_internal / workers_out_total`, where `workers_out_total` includes internal flows.}
#' }
#'
#' @description Derives three key commute flow metrics from an OD tibble:
#'   inflow, outflow, net flow, and the self-containment ratio. These metrics
#'   are widely used in transportation planning and economic geography to
#'   characterize labor market catchment areas and job/housing balance.
#'
#'   Self-containment is defined as the proportion of workers who both live
#'   and work within the same geographic unit, relative to all employed
#'   residents. A value close to 1 indicates a highly self-contained labor
#'   market; values near 0 indicate heavy out-commuting.
#'
#'   Net flow is defined as inbound workers minus outbound workers (including
#'   internal flows as both in and out). This is an unsigned flow balance
#'   indicator: positive values signal net job importers (more workers arrive
#'   than leave); negative values signal net exporters.
#'
#'   **Cross-state commuters:** When `state_part = "main"` is used in
#'   [grab_lodes()], only workers who live and work in the same state are
#'   included. Workers who cross state lines (e.g., Maryland residents working
#'   in DC) appear only in `state_part = "aux"` files for the *workplace*
#'   state. To capture full commute flows for border counties, retrieve both
#'   `"main"` and `"aux"` files and bind the rows before calling
#'   `compute_commute_stats()`.
#'
#' @importFrom dplyr group_by summarise left_join mutate select rename all_of coalesce
#' @importFrom rlang :=
#' @importFrom glue glue
#'
#' @examples
#' \donttest{
#'   od <- grab_lodes(
#'     state = "md", year = 2019,
#'     lodes_type = "od", job_type = "JT00",
#'     segment = "S000", state_part = "main",
#'     agg_geo = "county"
#'   )
#'   compute_commute_stats(od, agg_geo = "county")
#' }
#' @export
compute_commute_stats <- function(od_df, agg_geo = "tract") {
  agg_geo <- match.arg(
    agg_geo,
    choices = c("block", "bg", "tract", "county", "state")
  )

  h_col <- glue::glue("h_{if (agg_geo == 'block') 'geocode' else agg_geo}")
  w_col <- glue::glue("w_{if (agg_geo == 'block') 'geocode' else agg_geo}")
  flow_col <- "S000"

  for (col in c(h_col, w_col, flow_col)) {
    if (!col %in% names(od_df)) {
      rlang::abort(c(
        glue::glue("Column `{col}` not found in `od_df`."),
        "i" = glue::glue(
          "Ensure `od_df` was retrieved with `lodes_type = \"od\"` and ",
          "`agg_geo = \"{agg_geo}\"` (or aggregated to that level)."
        )
      ))
    }
  }

  # Inform the user of the pair-to-geography reduction
  n_pairs <- nrow(od_df)
  rlang::inform(glue::glue(
    "compute_commute_stats(): reducing {n_pairs} origin-destination pair",
    "{if (n_pairs == 1) '' else 's'} to one row per {agg_geo}."
  ))

  # Group-by columns present in od_df (year, state if available)
  grp_extras <- intersect(c("year", "state"), names(od_df))

  # Workers arriving at w_col geography (inflow)
  workers_in <- od_df %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(w_col, grp_extras)))) %>%
    dplyr::summarise(workers_in = sum(.data[[flow_col]], na.rm = TRUE),
                     .groups = "drop") %>%
    dplyr::rename(geo = dplyr::all_of(w_col))

  # Workers leaving h_col geography (outflow, includes internal)
  workers_out <- od_df %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(h_col, grp_extras)))) %>%
    dplyr::summarise(workers_out_total = sum(.data[[flow_col]], na.rm = TRUE),
                     .groups = "drop") %>%
    dplyr::rename(geo = dplyr::all_of(h_col))

  # Internal flows: home == work
  workers_internal <- od_df %>%
    dplyr::filter(.data[[h_col]] == .data[[w_col]]) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(h_col, grp_extras)))) %>%
    dplyr::summarise(workers_internal = sum(.data[[flow_col]], na.rm = TRUE),
                     .groups = "drop") %>%
    dplyr::rename(geo = dplyr::all_of(h_col))

  # Combine
  result <- workers_in %>%
    dplyr::full_join(workers_out, by = c("geo", grp_extras)) %>%
    dplyr::full_join(workers_internal, by = c("geo", grp_extras)) %>%
    dplyr::mutate(
      workers_in       = dplyr::coalesce(.data$workers_in, 0L),
      workers_out_total = dplyr::coalesce(.data$workers_out_total, 0L),
      workers_internal = dplyr::coalesce(.data$workers_internal, 0L),
      workers_out      = .data$workers_out_total - .data$workers_internal,
      net_flow         = .data$workers_in - .data$workers_out_total,
      self_containment = dplyr::if_else(
        .data$workers_out_total > 0,
        .data$workers_internal / .data$workers_out_total,
        NA_real_
      )
    ) %>%
    dplyr::select(
      "geo",
      dplyr::any_of(grp_extras),
      workers_in, workers_out, workers_internal,
      "net_flow", "self_containment"
    ) %>%
    dplyr::rename(!!agg_geo := "geo")

  return(result)
}
