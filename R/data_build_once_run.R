## =========================================================
## data_build_once_run.R
## Master script: veri hazirlama + duyarlilik analizi
##
## calistirma: RStudio'da ac, tamamini calistir (Ctrl+Shift+Enter)
## Tahmini süre: ~45-60 saat (i7-6700HQ, 16GB RAM)
## =========================================================

cat("\n", strrep("=", 60), "\n")
cat("DATA BUILD PIPELINE — STARTED\n")
cat("Time:", as.character(Sys.time()), "\n")
cat(strrep("=", 60), "\n\n")

## =========================================================
## ASAMA 1: SSP-bagimsiz paylasimli veriler (BIR KEZ)
## =========================================================
cat(">>> ASAMA 1: Shared data <<<\n\n")

source("R/02_data/build_bias_correction.R")
source("R/02_data/build_population_dataset.R")
source("R/02_data/build_travel_weights_zone.R")   # FIX: was build_travel_weights.R (nonexistent)
source("R/02_data/build_seasonality_monthly.R")
source("R/02_data/build_parameter_trait.R")
source("R/02_data/build_trait_params_species_files.R")
source("R/02_data/build_sentinel_species.R")

cat("\n>>> Asama 1 tamamlandi.\n\n")

## =========================================================
## ASAMA 2: SSP-bagimli veri hazirlama (HER SSP IcIN)
## =========================================================
cat(">>> ASAMA 2: SSP-dependent veri prep <<<\n\n")

SSP_LIST <- c("ssp126", "ssp245", "ssp585")

for (ssp in SSP_LIST) {
  cat("\n", strrep("-", 50), "\n")
  cat("  SSP:", ssp, "\n")
  cat(strrep("-", 50), "\n")

  Sys.setenv(SSP_SCENARIO = ssp)

  source("R/02_data/import_raw.R")
  source("R/02_data/clean_interim.R")
  source("R/02_data/build_climate_ssp.R")
  source("R/02_data/build_importation_pressure_monthly.R")
  source("R/02_data/build_rainfall_CSI.R")
}

cat("\n>>> Asama 2 tamamlandi.\n\n")


## =========================================================
## ASAMA 3: Duyarlilik analizi (k + η) (HER SSP IcIN)
##
## Base pipeline'in tamamlanmis ciktilarina baglidir.
## Bu yüzden Asama 2'den SONRA calisir.
## =========================================================
cat(">>> ASAMA 3: Duyarlilik analizi <<<\n\n")

for (ssp in SSP_LIST) {
  cat("\n  Sensitivity:", ssp, "\n")
  Sys.setenv(SSP_SCENARIO = ssp)
  source("R/02_data/run_sensitivity_importation.R")
}

cat("\n>>> Asama 3 tamamlandi.\n\n")

## =========================================================
## ASAMA 4: Model calistirmagetw (HER SSP IcIN)
##
## ctmc_spark_monte_carlo.R defines run_ctmc_spark() which:
##   - reads SSP-dependent climate + importation from DIR_PROCESSED_SSP
##   - writes model outputs to DIR_OUTPUT_SSP/simulation
##   - runs n_rep=1000 MC replications with stochastic EIP
##
## Tahmini süre: ~3-5 saat / SSP (i7-6700HQ, 16GB RAM)
## =========================================================
cat(">>> ASAMA 4: Model calistirma <<<\n\n")

for (ssp in SSP_LIST) {
  cat("\n  Model run:", ssp, "\n")
  Sys.setenv(SSP_SCENARIO = ssp)
  source("R/03_models/ctmc_spark_monte_carlo.R")
}

cat("\n>>> Asama 4 tamamlandi.\n\n")

## =========================================================
## ASAMA 5: Model duyarlilik analizi (HER SSP IcIN)
##
## sensitivity_ctmc_mc.R runs 9 scenarios × n_rep=1000 MC:
##   m ∈ {0.50, 0.80, 1.00, 1.20, 2.00}
##   beta_vh ± 20%, ip_days ± 20%
##
## Tahmini süre: ~20-30 saat / SSP (i7-6700HQ, 16GB RAM)
## Ilk test: n_rep=50 ile calistirarak dogrulayin.
## =========================================================
cat(">>> ASAMA 5: Model duyarlilik analizi <<<\n\n")

for (ssp in SSP_LIST) {
  cat("\n  Model sensitivity:", ssp, "\n")
  Sys.setenv(SSP_SCENARIO = ssp)
  source("R/03_models/sensitivity_ctmc_mc.R")
}
cat("\n>>> Asama 5 tamamlandi.\n\n")

cat("\n", strrep("=", 60), "\n")
cat("FULL PIPELINE — FINISHED (Stages 1-5)\n")
cat("Time:", as.character(Sys.time()), "\n")
cat(strrep("=", 60), "\n")
