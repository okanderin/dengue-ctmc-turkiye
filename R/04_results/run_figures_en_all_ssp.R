## =========================================================
## R/04_results/run_figures_en_all_ssp.R
## Driver: runs figures_en.R once per SSP scenario.
##
## WHY THIS FILE EXISTS
##   figures_en.R (like the Turkish original, 01_generate_ssp_outputs.R)
##   reads Sys.getenv("SSP_SCENARIO", unset = "ssp245") ONCE at the top
##   and generates figures/tables for that single scenario only. Running
##   figures_en.R directly (e.g. the RStudio Source button, or a bare
##   `Rscript figures_en.R`) therefore only ever produces the ssp245
##   ("unset" default) outputs -- ssp126 and ssp585 are never touched.
##
##   This driver reproduces the same for-loop pattern already used for
##   the Turkish pipeline in 02_figures_cross_scenario.R (its Section 0):
##   it sets the SSP_SCENARIO environment variable and re-sources
##   figures_en.R once per scenario, so all three SSPs' English figures
##   and tables get generated in one run.
##
## USAGE
##   Run this file instead of figures_en.R directly:
##     source("R/04_results/run_figures_en_all_ssp.R")
##   or from a terminal:
##     Rscript R/04_results/run_figures_en_all_ssp.R
##
## OUTPUT
##   outputs/ssp126/figures_en/ , outputs/ssp245/figures_en/ ,
##   outputs/ssp585/figures_en/  (and the matching tables_en/ folders),
##   each fully (re)populated -- existing PNGs with the same name are
##   overwritten, so re-running is always safe/idempotent.
## =========================================================

library(here)

SSP_LIST <- c("ssp126", "ssp245", "ssp585")   # must match the outputs/{ssp}/ folder names

for (ssp in SSP_LIST) {
  cat("\n", strrep("#", 60), "\n")
  cat("# Running figures_en.R for:", ssp, "\n")
  cat(strrep("#", 60), "\n")

  Sys.setenv(SSP_SCENARIO = ssp)
  source(here("R", "04_results", "figures_en.R"))
}

cat("\n", strrep("=", 60), "\n")
cat("DONE. English figures/tables generated for all SSP scenarios:\n")
for (ssp in SSP_LIST) {
  cat("  -", here("outputs", ssp, "figures_en"), "\n")
}
cat(strrep("=", 60), "\n")
