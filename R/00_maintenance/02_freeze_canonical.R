# =============================================================================
# 02_freeze_canonical.R                                     [ADIM 2 - DONDUR]
# -----------------------------------------------------------------------------
# Amac : Dogrulanmis ciktilari outputs/_canonical/ altina kopyalar ve
#        SHA-256 manifesti yazar. Bundan sonra Rmd YALNIZCA buradan okur.
#
# Onkosul : 01_audit_duplicates.R + 01b_audit_sensitivity.R kosuldu.
#           04_archive_legacy.R (APPLY=TRUE) uygulandi.
#
# Kaynak secimleri (audit sonucu):
#   simulation/  <- outputs/{ssp}/simulation/            (MC, rep1000, Agustos)
#   analytic/    <- outputs/{ssp}/model_results/         (ctmc_spark.R, Agustos)
#   oat/         <- outputs/{ssp}/sensitivity/ctmc_mc/   (Agustos, post-Poisson)
#                   NOT: ctmc_mc_rep1000/ (Temmuz, Bernoulli) KULLANILMAZ
#   importation/ <- outputs/{ssp}/sensitivity/importation/*/  (2025-2075 ufku)
#   tables/      <- outputs/tables/                      (regresyon, kvpd, ek A4)
# =============================================================================

library(here); library(dplyr); library(readr); library(digest); library(purrr)

SSPS  <- c("ssp126", "ssp245", "ssp585")
CANON <- here("outputs", "_canonical")

# --- KAYNAK YOLLARI ----------------------------------------------------------
MC_SRC <- function(ssp) here("outputs", ssp, "simulation")

ANALYTIC_SRC <- function(ssp) {
  a <- here("outputs", ssp, "model_results")
  b <- here("outputs", "frozen", ssp, "model_results")
  if (dir.exists(a) && length(list.files(a, "\\.csv$")) > 0) a else b
}

# --- DOSYA LISTELERI ---------------------------------------------------------
FILES_MC <- c(
  "ctmc_spark_horizon_2025_2075_rep1000.csv",
  "ctmc_spark_yearly_2025_2075_rep1000.csv",
  "ctmc_spark_monthly_2025_2075_rep1000.csv"
)

FILES_ANALYTIC <- c(
  "ctmc_spark_horizon_2025_2075.csv",
  "ctmc_spark_yearly_2025_2075.csv",
  "ctmc_spark_monthly_2025_2075.csv"
)

FILES_SSP_TABLES <- c(
  "tbl_horizon.csv", "tbl_decade.csv", "tbl_season.csv", "tbl_moran.csv",
  "tbl_prcc.csv", "tbl_prcc_param_ranges.csv", "tbl_param_ranges.csv",
  "tbl_import_summary.csv", "tbl_country_contrib.csv",
  "tbl_csi_monthly.csv", "tbl_sens_importation.csv", "tbl_validation.csv"
)

# Yalnizca bazi SSP'lerde bulunanlar — eksikse uyari verilmez
FILES_SSP_OPTIONAL <- c(
  "tbl_eip_sdlog_district_month.csv",
  "tbl_csi_monthly_SSP1-2.6_tr.csv",
  "tbl_csi_monthly_SSP2-4.5_tr.csv",
  "tbl_csi_monthly_SSP5-8.5_tr.csv"
)

OAT_SCEN <- c("base", "m_050", "m_080", "m_120", "m_200",
              "beta_minus_20", "beta_plus_20", "ip_minus_20", "ip_plus_20")

OAT_SUMMARY <- c("sensitivity_params.csv", "sensitivity_summary.csv",
                 "sensitivity_tornado.csv")

IMPORT_SCEN <- c("eta_low", "eta_base", "eta_high", "k010", "k013", "k020")
IMPORT_FILE <- "importation_pressure_monthly_2025_2075.csv"

FILES_CROSS_TABLES <- c(
  "topt_mechanistic.csv", "topt_comparison.csv", "cross_ssp_topt.csv",
  "tobit_fe_coefficients.csv", "ols_coefficients.csv", "ols_hierarchical.csv",
  "ols_active_diagnostic.csv", "ols_lmm_comparison.csv",
  "lmm_fit_metrics.csv", "lmm_fixed_effects.csv",
  "lmm_r2_nakagawa.csv", "lmm_ri_secondary.csv",
  "tbl_ek_a4_poisson_bernoulli.csv",
  "tbl_kvpd_sensitivity.csv", "tbl_kvpd_reference_check.csv"
)

# --- GUVENLIK ----------------------------------------------------------------
if (dir.exists(CANON) && length(list.files(CANON, recursive = TRUE)) > 0)
  stop("outputs/_canonical/ dolu. Bilerek sil ya da _canonical_v2 kullan.")

# --- KOPYALAMA ---------------------------------------------------------------
missing_log <- character()

copy_one <- function(from, to, optional = FALSE) {
  if (!file.exists(from)) {
    if (!optional) {
      warning("MISSING: ", from, call. = FALSE)
      missing_log <<- c(missing_log, from)
    }
    return(NULL)
  }
  dir.create(dirname(to), recursive = TRUE, showWarnings = FALSE)
  if (!file.copy(from, to, overwrite = FALSE)) {
    warning("COPY FAILED: ", to, call. = FALSE)
    missing_log <<- c(missing_log, paste("COPY FAILED:", to))
    return(NULL)
  }
  tibble(rel_path = sub(paste0("^", CANON, "/?"), "", to),
         source   = from,
         bytes    = file.info(to)$size,
         sha256   = digest(file = to, algo = "sha256"))
}

man <- list()
add <- function(x) if (!is.null(x)) man[[length(man) + 1]] <<- x

for (ssp in SSPS) {
  
  # 1. Monte Carlo ana kosum (rep1000)
  for (f in FILES_MC)
    add(copy_one(file.path(MC_SRC(ssp), f),
                 file.path(CANON, ssp, "simulation", f)))
  
  # 2. Analitik cikti (gambler's ruin) — MC dogrulamasi icin, ayri urun
  for (f in FILES_ANALYTIC)
    add(copy_one(file.path(ANALYTIC_SRC(ssp), f),
                 file.path(CANON, ssp, "analytic", f)))
  
  # 3. Per-SSP tablolar
  for (f in FILES_SSP_TABLES)
    add(copy_one(here("outputs", ssp, "tables", f),
                 file.path(CANON, ssp, "tables", f)))
  
  for (f in FILES_SSP_OPTIONAL)
    add(copy_one(here("outputs", ssp, "tables", f),
                 file.path(CANON, ssp, "tables", f), optional = TRUE))
  
  # 4. OAT senaryolari
  for (sc in OAT_SCEN)
    add(copy_one(
      here("outputs", ssp, "sensitivity", "ctmc_mc", sc,
           "ctmc_spark_horizon_2025_2075_rep1000.csv"),
      file.path(CANON, ssp, "oat", sc,
                "ctmc_spark_horizon_2025_2075_rep1000.csv")))
  
  # 5. OAT ozet tablolari
  for (f in OAT_SUMMARY)
    add(copy_one(here("outputs", ssp, "sensitivity", "ctmc_mc", f),
                 file.path(CANON, ssp, "oat", f)))
  
  # 6. Ithalat duyarliligi (eta, k)
  for (sc in IMPORT_SCEN)
    add(copy_one(
      here("outputs", ssp, "sensitivity", "importation", sc, IMPORT_FILE),
      file.path(CANON, ssp, "importation", sc, IMPORT_FILE)))
}

# 7. Cross-SSP tablolar (regresyon, LMM, Tobit, kvpd, Ek A4)
for (f in FILES_CROSS_TABLES)
  add(copy_one(here("outputs", "tables", f),
               file.path(CANON, "tables", f)))

# --- MANIFEST ----------------------------------------------------------------
manifest <- bind_rows(man) %>%
  mutate(frozen_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"))

write_csv(manifest, file.path(CANON, "MANIFEST.csv"))

writeLines(c(
  paste("frozen_at   :", manifest$frozen_at[1]),
  paste("R version   :", R.version.string),
  paste("n_files     :", nrow(manifest)),
  paste("MC src      :", MC_SRC("ssp245")),
  paste("ANALYTIC src:", ANALYTIC_SRC("ssp245")),
  paste("OAT src     :", here("outputs", "{ssp}", "sensitivity", "ctmc_mc")),
  "",
  "DIZIN ANLAMLARI",
  "  simulation/  = ctmc_spark_monte_carlo.R  (stokastik, n_rep=1000)",
  "  analytic/    = ctmc_spark.R              (gambler's ruin, deterministik)",
  "  oat/         = sensitivity_ctmc_mc.R     (tek-parametre-degisim)",
  "  importation/ = run_sensitivity_importation.R (eta, k)",
  "  tables/      = 06_10_pest_regression.R + kvpd + Ek A4",
  "",
  "KURAL",
  "  bulgular_yeni_tam.Rmd YALNIZCA outputs/_canonical/ okur.",
  "  Rmd icinde simulasyon cagrisi = hata.",
  "",
  "NOT",
  "  OAT'ta m=0.80, beta_vh-%20 ve IP-%20 ozdes P_ufuk verir; lambda bu uc",
  "  parametrede dogrusal oldugundan beklenen davranistir (artefakt degil)."
), file.path(CANON, "PROVENANCE.txt"))

# Salt-okunur yap (Windows)
#if (.Platform$OS.type == "windows")
 # invisible(system2("attrib", c("+R", shQuote(file.path(CANON, "*.*")), "/S")))

# --- RAPOR -------------------------------------------------------------------
cat("\n=== DONDURMA TAMAMLANDI ===\n")
cat("Dosya sayisi: ", nrow(manifest), "\n", sep = "")
cat("Hedef       : ", CANON, "\n\n", sep = "")

manifest %>%
  mutate(dir = dirname(rel_path)) %>%
  count(dir, name = "n") %>%
  arrange(dir) %>% as.data.frame() %>% print(row.names = FALSE)

if (length(missing_log) > 0) {
  cat("\n!! EKSIK / BASARISIZ (", length(missing_log), "):\n", sep = "")
  cat(paste0("   ", missing_log, collapse = "\n"), "\n")
} else {
  cat("\nEksik dosya yok.\n")
}

cat("\nManifest: ", file.path(CANON, "MANIFEST.csv"), "\n", sep = "")