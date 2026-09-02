## =========================================================
## R/04_results/02_figures_cross_scenario_en.R
## English twin of R/04_results/core/02_figures_cross_scenario.R.
##
## WHAT THIS DOES
##   Combines each English figure's three SSP-scenario panels
##   (outputs/{ssp}/figures_en/<name>.png, ssp = ssp126/ssp245/ssp585)
##   into one vertically stacked, 300-DPI image per figure -- the same
##   "read three PNGs -> rasterGrob -> wrap_plots(ncol = 1) -> ggsave"
##   approach as the Turkish original. Panel labels/legends are already
##   baked into the source PNGs by figures_en.R, so no text is set or
##   translated here -- this script only lays images out.
##
## PRECONDITION
##   The three SSP scenarios' English figures must already exist, i.e.
##   R/04_results/run_figures_en_all_ssp.R must have been run first (or
##   Sys.setenv(SSP_SCENARIO=...) ; source("figures_en.R") for each of
##   ssp126/ssp245/ssp585 by hand). Section 0 below re-runs that driver
##   automatically before combining, exactly as the Turkish original's
##   Section 0 re-runs 01_generate_ssp_outputs.R for all three SSPs
##   before combining -- so combined figures are never built from stale
##   source PNGs left over from a previous run.
##
## USAGE
##   source("R/04_results/02_figures_cross_scenario_en.R")
##
## OUTPUT
##   One combined PNG per figure in DIR_OUTPUT_CROSS_EN (defaults to
##   outputs/cross_scenario_en/ if DIR_OUTPUT_CROSS is not already
##   defined by init.R/paths.R -- see FALLBACK note below), named
##   combined_plot_..._en.png (an "_en" suffix is added throughout so
##   nothing here ever overwrites the Turkish combined outputs in
##   DIR_OUTPUT_CROSS).
## =========================================================

library(magick)
library(patchwork)
library(ggplot2)
library(grid)
library(here)

SSP_LIST <- c("ssp126", "ssp245", "ssp585")   # must match the outputs/{ssp}/ folder names

## FALLBACK: the Turkish original assumes DIR_OUTPUT_CROSS is already
## defined by a previously sourced init.R/paths.R. If this script is run
## on its own without that precondition, define a sensible English
## default instead of failing.
if (!exists("DIR_OUTPUT_CROSS")) {
  DIR_OUTPUT_CROSS <- here("outputs", "cross_scenario")
}
DIR_OUTPUT_CROSS_EN <- here("outputs", "cross_scenario_en")
dir.create(DIR_OUTPUT_CROSS_EN, recursive = TRUE, showWarnings = FALSE)

# ---------------------------------------------------------
# 0) (Re)generate the source English figures for all three scenarios
#    FIRST, so no combined figure is ever built from a stale source PNG.
#    Mirrors the Turkish original's Section 0 exactly, but drives
#    figures_en.R instead of 01_generate_ssp_outputs.R.
# ---------------------------------------------------------
for (ssp in SSP_LIST) {
  Sys.setenv(SSP_SCENARIO = ssp)
  source(here("R", "04_results", "figures_en.R"))
}

# ---------------------------------------------------------
# Helper: read the given figure for each SSP from figures_en/, turn it
# into a grob, stack vertically (ncol = 1), and save at 300 DPI.
# (rasterGrob tiles already-rendered PNGs -- panel labels/legends are
#  baked into the source figure, no text is set here.)
# A missing file skips that scenario; if none exist, the figure is
# skipped with a warning.
# ---------------------------------------------------------
combine_vertical_en <- function(fig_name, out_name, width = 10, height = 15) {
  grobs <- lapply(SSP_LIST, function(s) {
    path <- here("outputs", s, "figures_en", fig_name)
    if (file.exists(path)) rasterGrob(image_read(path), interpolate = TRUE) else NULL
  })
  grobs <- grobs[!vapply(grobs, is.null, logical(1))]
  if (length(grobs) == 0L) {
    warning("No figure found, skipping: ", fig_name)
    return(invisible(NULL))
  }
  combined <- wrap_plots(grobs) + plot_layout(ncol = 1)
  ggsave(
    filename = here(DIR_OUTPUT_CROSS_EN, out_name),
    plot = combined, dpi = 300, width = width, height = height, units = "in"
  )
  message(sprintf("Saved: %-45s (%d panel%s)", out_name, length(grobs), if (length(grobs) == 1) "" else "s"))
  invisible(combined)
}

# ---------------------------------------------------------
# All scenario-panelled figures to combine.
# (Source filename identical to figures_en.R's ggsave() names; to add a
#  new figure, add one line here.)
# ---------------------------------------------------------
fig_specs_en <- list(
  # source filename              output filename                                    w    h
  list("fig_temp_rh.png",        "combined_plot_temp_rh_profile_vertical_en.png",   9, 18),
  list("fig_heatmap_lambda.png", "combined_plot_heatmap_lambda_en.png",            12, 15),
  list("fig_pest_season.png",    "combined_plot_pest_season_en.png",               10, 15),
  list("fig_yearly_risk.png",    "combined_plot_yearly_risk_en.png",                9, 15),
  list("fig_import_trend.png",   "combined_plot_import_trend_en.png",               9, 15),
  list("fig_import_heatmap.png", "combined_plot_import_heatmap_en.png",            12, 15),
  list("fig_validation.png",     "combined_plot_validation_en.png",                12, 15),
  list("fig_prcc.png",           "combined_plot_prcc_en.png",                       9, 15),
  list("fig_csi_heat.png",       "combined_plot_csi_heat_en.png",                  10, 15),
  list("fig_csi_trend.png",      "combined_plot_csi_trend_en.png",                  9, 15),
  list("fig_decade.png",         "combined_plot_decade_en.png",                     9, 15),
  list("fig_season.png",         "combined_plot_season_en.png",                     9, 15),
  # generated inside conditional blocks in figures_en.R (only if the
  # relevant package is installed); skipped automatically if absent
  list("fig_tornado.png",         "combined_plot_tornado_en.png",                   9, 15),
  list("fig_country_contrib.png", "combined_plot_country_contrib_en.png",           9, 15),
  list("fig_moran.png",           "combined_plot_moran_en.png",                     9, 15)
)

for (spec in fig_specs_en) {
  combine_vertical_en(fig_name = spec[[1]], out_name = spec[[2]],
                      width = spec[[3]], height = spec[[4]])
}

cat("\nCombined English figures written to:", DIR_OUTPUT_CROSS_EN, "\n")
