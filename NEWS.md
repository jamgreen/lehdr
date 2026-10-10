# lehdr 1.2.1

* `grab_lodes()` now accepts `year = 2023` for LODES8, following the Census
  Bureau's release of 2023 LODES data. LODES8 now covers 2002-2023 (#46).
* Documentation for the LODES8 year range is updated in `grab_lodes()` and its
  Rd file.
* Added a regression test that downloads 2023 LODES8 WAC data for Massachusetts
  and checks the total against the published file. Thanks to @stevenpandrews
  for the test (#46).
* Tests for out-of-range years now use fixed years (2001 and 2099) so they do
  not need editing when new LODES years are released. Tests now cover both
  the lower and upper bounds for LODES8.
* Examples that download data from the Census Bureau server are now wrapped in
  `\dontrun{}` so that `R CMD check` does not depend on network availability.
* The `R-CMD-check` workflow no longer fails when the R-devel job on Ubuntu
  stalls, and installed vignette figures no longer trigger an `inst/doc` NOTE.
* License and citation years are updated to 2026.

# lehdr 1.2.0

* Input validation for `state`, `year`, `use_cache`, and `state_part` is
  stricter and error messages are consistent (`rlang::abort()`).
* Download logic shared across LODES file types is consolidated into an
  internal helper.
* `grab_crosswalk()` gains a `version` argument.
* Cached files now include the LODES version in the file name (for example,
  `lodes8_md_wac_S000_JT00_2019.csv.gz`). Older cached files are not matched
  and can be deleted.
* New analytical functions: `compute_commute_stats()` (self-containment and net
  flow), `compute_lodes_change()` (longitudinal change), and
  `compute_earnings_share()` (earnings tier distribution).
* The Getting Started vignette is expanded to cover the new functions, with an
  example of a job accessibility index.
