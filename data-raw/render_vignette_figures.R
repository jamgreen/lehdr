# data-raw/render_vignette_figures.R
#
# Pre-renders all figures used in vignettes/getting_started.Rmd.
# Run this script manually when figures need to be refreshed.
# Output PNGs are committed to vignettes/figures/ and referenced
# by the vignette via knitr::include_graphics().
#
# This script is excluded from the CRAN package build via .Rbuildignore.
#
# Requirements (all in Suggests):
#   ggplot2, sf, tigris, dplyr, scales

library(lehdr)
library(dplyr)
library(ggplot2)
library(sf)
library(tigris)
library(scales)

options(lehdr_use_cache = TRUE)
options(tigris_use_cache = TRUE)

fig_dir <- here::here("vignettes", "figures")
if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE)

save_fig <- function(plot, filename, width = 7, height = 5, dpi = 150) {
  path <- file.path(fig_dir, filename)
  ggsave(path, plot = plot, width = width, height = height, dpi = dpi)
  message("Saved: ", path)
}

# ── Shared theme ──────────────────────────────────────────────────────────────

map_theme <- theme_void(base_size = 11) +
  theme(
    plot.title        = element_text(face = "bold", size = 12, margin = margin(b = 6)),
    plot.subtitle     = element_text(size = 10, color = "grey40", margin = margin(b = 10)),
    plot.caption      = element_text(size = 8, color = "grey50", margin = margin(t = 8)),
    legend.position   = "right",
    legend.title      = element_text(size = 9, face = "bold"),
    legend.text       = element_text(size = 8),
    plot.margin       = margin(10, 10, 10, 10)
  )

# ── Figure 1: Self-containment, Baltimore City tracts ─────────────────────────
# Uses OD data at the tract level filtered to Baltimore City (FIPS 24510).
# compute_commute_stats() derives the self-containment ratio.

message("Figure 1: Baltimore City self-containment...")

od_balt <- grab_lodes(
  state      = "md",
  year       = 2019,
  lodes_type = "od",
  job_type   = "JT00",
  segment    = "S000",
  state_part = "main",
  agg_geo    = "tract"
)

# Keep only tracts whose home geography is in Baltimore City
balt_tracts_od <- od_balt %>%
  filter(startsWith(h_tract, "24510"))

commute_balt <- compute_commute_stats(balt_tracts_od, agg_geo = "tract")

# Pull tract geometries for Baltimore City
balt_geo <- tracts(state = "MD", county = "510", year = 2019, cb = TRUE) %>%
  select(GEOID, geometry)

commute_map_df <- balt_geo %>%
  left_join(commute_balt, by = c("GEOID" = "tract"))

fig1 <- ggplot(commute_map_df) +
  geom_sf(aes(fill = self_containment), color = "white", linewidth = 0.2) +
  scale_fill_distiller(
    palette  = "YlOrRd",
    direction = 1,
    name     = "Self-\ncontainment",
    labels   = label_percent(accuracy = 1),
    na.value = "grey85"
  ) +
  labs(
    title    = "Worker self-containment, Baltimore City",
    subtitle = "Share of employed residents who also work within the same Census tract",
    caption  = "Source: U.S. Census Bureau LODES 2019 via lehdr"
  ) +
  map_theme

save_fig(fig1, "fig1_balt_self_containment.png", width = 6.5, height = 6)


# ── Figure 2: Job change 2010–2019, Maryland counties ─────────────────────────
# WAC data for two years; compute_lodes_change() returns pct change in C000.

message("Figure 2: Maryland job change 2010-2019...")

wac_md_panel <- grab_lodes(
  state      = "md",
  year       = c(2010, 2019),
  lodes_type = "wac",
  job_type   = "JT00",
  segment    = "S000",
  agg_geo    = "county"
)

change_md <- compute_lodes_change(
  wac_md_panel,
  geo_col      = "w_county",
  base_year    = 2010,
  compare_year = 2019,
  variables    = "C000"
)

# cb = FALSE required: cartographic boundary (cb = TRUE) drops NAMELSAD, keeping only NAME,
# which causes Baltimore city and Baltimore County to both appear as "Baltimore".
# Full TIGER file (cb = FALSE) preserves NAMELSAD for correct labeling.
md_county_geo <- counties(state = "MD", year = 2019, cb = FALSE) %>%
  mutate(GEOID = paste0(STATEFP, COUNTYFP)) %>%
  select(GEOID, geometry)

change_map_df <- md_county_geo %>%
  left_join(change_md, by = c("GEOID" = "w_county"))

fig2 <- ggplot(change_map_df) +
  geom_sf(aes(fill = C000_pct_change), color = "white", linewidth = 0.3) +
  scale_fill_distiller(
    palette   = "RdYlBu",
    direction = 1,
    name      = "% change",
    labels    = label_percent(accuracy = 1, scale = 1),
    na.value  = "grey85"
  ) +
  labs(
    title    = "Job growth by county, Maryland, 2010\u20132019",
    subtitle = "Percent change in total jobs (WAC, all job types)",
    caption  = "Source: U.S. Census Bureau LODES via lehdr"
  ) +
  map_theme

save_fig(fig2, "fig2_md_job_change.png", width = 7.5, height = 5)


# ── Figure 3: Earnings tier shares, Maryland counties ─────────────────────────
# Stacked bar showing low/mid/high shares per county, sorted by share_low.

message("Figure 3: Maryland earnings tier shares...")

wac_md_2019 <- grab_lodes(
  state      = "md",
  year       = 2019,
  lodes_type = "wac",
  job_type   = "JT00",
  segment    = "S000",
  agg_geo    = "county"
)

earn_shares <- compute_earnings_share(
  wac_md_2019,
  type    = "wac",
  geo_col = "w_county",
  output  = "long"
)

# Attach county names via tigris for readable labels
# cb = FALSE required: cartographic boundary (cb = TRUE) drops NAMELSAD, keeping only NAME,
# which causes Baltimore city and Baltimore County to both appear as "Baltimore".
# Full TIGER file (cb = FALSE) preserves NAMELSAD for correct labeling.
md_county_names <- counties(state = "MD", year = 2019, cb = FALSE) %>%
  as_tibble() %>%
  mutate(GEOID = paste0(STATEFP, COUNTYFP)) %>%
  select(GEOID, NAMELSAD)

earn_shares <- earn_shares %>%
  left_join(md_county_names, by = c("w_county" = "GEOID")) %>%
  rename(NAME = NAMELSAD) %>%
  mutate(
    tier = factor(tier, levels = c("high", "mid", "low"),
                  labels = c("High (> $3,333/mo)", "Mid ($1,251\u2013$3,333/mo)",
                             "Low (\u2264 $1,250/mo)")),
    NAME = reorder(NAME, ifelse(tier == "Low (\u2264 $1,250/mo)", share, NA),
                   FUN = function(x) mean(x, na.rm = TRUE))
  )

fig3 <- ggplot(earn_shares, aes(x = NAME, y = share, fill = tier)) +
  geom_col(width = 0.75) +
  scale_fill_manual(
    values = c(
      "High (> $3,333/mo)"         = "#2166ac",
      "Mid ($1,251\u2013$3,333/mo)" = "#92c5de",
      "Low (\u2264 $1,250/mo)"      = "#d6604d"
    ),
    name = "Earnings tier"
  ) +
  scale_y_continuous(labels = label_percent(accuracy = 1)) +
  coord_flip() +
  labs(
    title    = "Earnings tier composition by county, Maryland, 2019",
    subtitle = "Share of jobs in each monthly earnings tier (WAC, all job types)",
    caption  = "Source: U.S. Census Bureau LODES via lehdr",
    x        = NULL,
    y        = "Share of jobs"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title      = element_text(face = "bold", size = 12),
    plot.subtitle   = element_text(size = 10, color = "grey40"),
    plot.caption    = element_text(size = 8, color = "grey50"),
    legend.position = "bottom",
    panel.grid.major.y = element_blank()
  )

save_fig(fig3, "fig3_md_earnings_shares.png", width = 7.5, height = 6.5)


# ── Figure 4: Job accessibility, Baltimore City tracts ────────────────────────
# Flow-weighted reachable jobs from each home tract in Baltimore City.

message("Figure 4: Baltimore City job accessibility...")

od_md_tract <- grab_lodes(
  state      = "md",
  year       = 2019,
  lodes_type = "od",
  job_type   = "JT00",
  segment    = "S000",
  state_part = "main",
  agg_geo    = "tract"
)

accessibility <- od_md_tract %>%
  filter(startsWith(h_tract, "24510")) %>%
  group_by(h_tract) %>%
  summarise(
    n_dest          = n_distinct(w_tract),
    total_reachable = sum(S000, na.rm = TRUE),
    .groups         = "drop"
  )

access_map_df <- balt_geo %>%
  left_join(accessibility, by = c("GEOID" = "h_tract"))

fig4 <- ggplot(access_map_df) +
  geom_sf(aes(fill = total_reachable), color = "white", linewidth = 0.2) +
  scale_fill_distiller(
    palette   = "YlOrRd",
    direction = 1,
    name      = "Workers\n(flow-weighted)",
    labels    = label_comma(accuracy = 1),
    na.value  = "grey85"
  ) +
  labs(
    title    = "Job accessibility, Baltimore City tracts, 2019",
    subtitle = "Flow-weighted count of jobs reachable from each home Census tract",
    caption  = "Source: U.S. Census Bureau LODES via lehdr"
  ) +
  map_theme

save_fig(fig4, "fig4_balt_accessibility.png", width = 6.5, height = 6)

message("All figures saved to vignettes/figures/")
