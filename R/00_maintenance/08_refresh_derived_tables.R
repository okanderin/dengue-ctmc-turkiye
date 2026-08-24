# =============================================================================
# 08_refresh_derived_tables.R
# -----------------------------------------------------------------------------
# AMAC
#   Analiz betikleri yeniden kosuldugunda urettikleri tablolari
#   outputs/_canonical/ altina tazeler ve MANIFEST'i gunceller.
#
# NEDEN
#   02_freeze_canonical.R bir kereliktir. Bir betigi (or. kvpd_sensitivity_check.R,
#   06_10_pest_regression.R, 01_generate_ssp_outputs.R) yeniden kosarsan yeni
#   cikti outputs/ altina yazilir; Rmd ise outputs/_canonical/ okur. Bu betik
#   kosulmazsa Rmd ESKI dosyayi gosterir ve bu sessiz bir hatadir.
#
# NE DONDURULMAZ
#   Simulasyon ciktilari (simulation/, analytic/, oat/) BILEREK disaridadir.
#   Onlar yalnizca 02_freeze_canonical.R ile, bilinçli bir kararla degisir.
#
# KOSUM
#   source(here::here("R","00_maintenance","08_refresh_derived_tables.R"))
# =============================================================================

library(here); library(dplyr); library(readr); library(digest); library(purrr)
library(tibble); library(tidyr)

CANON <- here("outputs", "_canonical")
SSPS  <- c("ssp126", "ssp245", "ssp585")

if (!dir.exists(CANON)) stop("outputs/_canonical/ yok. Once 02_freeze_canonical.R kos.")

# Salt-okunur kilidini ac
if (.Platform$OS.type == "windows")
  invisible(suppressWarnings(system2("attrib",
                                     c("-R", shQuote(file.path(CANON, "*.*")), "/S"), stdout = FALSE, stderr = FALSE)))

# --- Tazelenecek dosyalar ----------------------------------------------------

# (A) Kok outputs/tables/ — senaryolar arasi tablolar
CROSS_TABLES <- c(
  # k_vpd duyarliligi (kvpd_sensitivity_check.R)
  "tbl_kvpd_sensitivity.csv", "tbl_kvpd_reference_check.csv",
  "tbl_kvpd_canonical_check.csv", "tbl_kvpd_hierarchy.csv",
  # Regresyon (06_10_pest_regression.R)
  "topt_mechanistic.csv", "topt_comparison.csv", "cross_ssp_topt.csv",
  "tobit_fe_coefficients.csv", "ols_coefficients.csv", "ols_hierarchical.csv",
  "ols_active_diagnostic.csv", "ols_lmm_comparison.csv",
  "lmm_fit_metrics.csv", "lmm_fixed_effects.csv",
  "lmm_r2_nakagawa.csv", "lmm_ri_secondary.csv",
  # Ek A4 (ek_a4_poisson_bernoulli_check.R)
  "tbl_ek_a4_poisson_bernoulli.csv"
)

# (B) Per-SSP outputs/{ssp}/tables/ — 01_generate_ssp_outputs.R ciktilari
SSP_TABLES <- c(
  "tbl_horizon.csv", "tbl_decade.csv", "tbl_season.csv", "tbl_moran.csv",
  "tbl_prcc.csv", "tbl_prcc_param_ranges.csv", "tbl_param_ranges.csv",
  "tbl_import_summary.csv", "tbl_country_contrib.csv",
  "tbl_csi_monthly.csv", "tbl_sens_importation.csv", "tbl_validation.csv"
)

SSP_TABLES_OPT <- c(
  "tbl_eip_sdlog_district_month.csv",
  "tbl_csi_monthly_SSP1-2.6_tr.csv", "tbl_csi_monthly_SSP2-4.5_tr.csv",
  "tbl_csi_monthly_SSP5-8.5_tr.csv"
)

# (C) Per-SSP dogrulama ve tani ciktilari
SSP_VALIDATION <- c("validation_stage1_summary.csv", "validation_stage2_jensen.csv")
SSP_DIAG       <- c("mc_validation_summary.csv")

# (D) Figurler — yeniden uretilenler
CROSS_FIGS <- c(
  "fig_oat_tornado_pp.png", "fig_oat_m_profile_pp.png",
  "combined_fig_prcc.png", "validation_all_ssp.png",
  "fig_yearly_risk_comparison.png", "fig_decade_progression_3ssp.png",
  "fig_forest.png", "fig_pest_season_cross.png",
  "combined_plot_csi_heat_vertical_tr.png",
  "combined_plot_temp_rh_profile_vertical.png",
  "fig_taylor_validation.png", "fig_kartal_taylor_profile.png",
  "fig_A1_poisson_validation.png", "fig_R0_dagilim.png"
)

# --- Kopyalama ---------------------------------------------------------------
refresh_one <- function(from, to, optional = FALSE) {
  if (!file.exists(from))
    return(tibble(rel_path = sub(paste0("^", CANON, "/?"), "", to),
                  durum = if (optional) "opsiyonel-yok" else "KAYNAK YOK",
                  sha256 = NA_character_, bytes = NA_real_))
  
  eski <- if (file.exists(to)) digest(file = to, algo = "sha256") else NA_character_
  dir.create(dirname(to), recursive = TRUE, showWarnings = FALSE)
  if (!file.copy(from, to, overwrite = TRUE))
    return(tibble(rel_path = sub(paste0("^", CANON, "/?"), "", to),
                  durum = "KOPYA BASARISIZ", sha256 = NA_character_, bytes = NA_real_))
  
  yeni <- digest(file = to, algo = "sha256")
  tibble(rel_path = sub(paste0("^", CANON, "/?"), "", to),
         durum = if (is.na(eski)) "YENI"
         else if (eski != yeni) "GUNCELLENDI" else "degismedi",
         sha256 = yeni, bytes = file.info(to)$size, source = from)
}

log <- list(); add <- function(x) log[[length(log) + 1]] <<- x

# (A) kok tablolar
for (f in CROSS_TABLES)
  add(refresh_one(here("outputs", "tables", f), file.path(CANON, "tables", f)))

# (B)(C) per-SSP
for (ssp in SSPS) {
  for (f in SSP_TABLES)
    add(refresh_one(here("outputs", ssp, "tables", f),
                    file.path(CANON, ssp, "tables", f)))
  for (f in SSP_TABLES_OPT)
    add(refresh_one(here("outputs", ssp, "tables", f),
                    file.path(CANON, ssp, "tables", f), optional = TRUE))
  for (f in SSP_VALIDATION)
    add(refresh_one(here("outputs", ssp, "validation", f),
                    file.path(CANON, ssp, "validation", f)))
  for (f in SSP_DIAG)
    add(refresh_one(here("outputs", ssp, "diagnostics", "mc_validation", f),
                    file.path(CANON, ssp, "diagnostics", "mc_validation", f)))
  # per-SSP figurler (tumu)
  fig_src <- here("outputs", ssp, "figures")
  if (dir.exists(fig_src))
    for (p in list.files(fig_src, "\\.png$", full.names = TRUE))
      add(refresh_one(p, file.path(CANON, ssp, "figures", basename(p))))
}

# (D) cross figurler
for (f in CROSS_FIGS)
  add(refresh_one(here("outputs", "cross_scenario", f),
                  file.path(CANON, "cross_scenario", f), optional = TRUE))

# OAT turetilmis per-SSP tablolar (07_rebuild_oat_tables.R zaten _canonical'a yazar)
for (ssp in SSPS)
  for (f in c("tbl_oat_pufuk.csv", "tbl_oat_delta_pp.csv")) {
    p <- file.path(CANON, ssp, "oat", f)
    if (file.exists(p))
      add(tibble(rel_path = sub(paste0("^", CANON, "/?"), "", p),
                 durum = "yerinde", sha256 = digest(file = p, algo = "sha256"),
                 bytes = file.info(p)$size, source = p))
  }

logd <- bind_rows(log)

# --- MANIFEST guncelle -------------------------------------------------------
man <- read_csv(file.path(CANON, "MANIFEST.csv"), show_col_types = FALSE)

upd <- logd %>% filter(!is.na(sha256)) %>%
  transmute(rel_path, source = if ("source" %in% names(.)) source else NA_character_,
            bytes, sha256,
            frozen_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"))

out <- bind_rows(anti_join(man, upd, by = "rel_path"), upd) %>% arrange(rel_path)
write_csv(out, file.path(CANON, "MANIFEST.csv"))

# --- RAPOR -------------------------------------------------------------------
cat("=== TAZELEME OZETI ===\n")
logd %>% count(durum) %>% as.data.frame() %>% print(row.names = FALSE)

deg <- logd %>% filter(durum %in% c("GUNCELLENDI", "YENI"))
if (nrow(deg) > 0) {
  cat("\n--- Degisen / eklenen dosyalar ---\n")
  cat(paste0("  [", deg$durum, "] ", deg$rel_path, collapse = "\n"), "\n")
}

eks <- logd %>% filter(durum %in% c("KAYNAK YOK", "KOPYA BASARISIZ"))
if (nrow(eks) > 0) {
  cat("\n!! SORUNLU (", nrow(eks), "):\n", sep = "")
  cat(paste0("  [", eks$durum, "] ", eks$rel_path, collapse = "\n"), "\n")
}

cat("\nManifest: ", nrow(out), " dosya\n", sep = "")

# --- DOGRULAMA: k_vpd tablosu ana hatla uyusuyor mu? -------------------------
cat("\n=== KONTROL: kanonik k_vpd tablosu, Kartal k=0.5 ===\n")
kf <- file.path(CANON, "tables", "tbl_kvpd_sensitivity.csv")
if (file.exists(kf)) {
  kd <- read_csv(kf, show_col_types = FALSE)
  ic <- intersect(c("Ilce", "district", "İlçe"), names(kd))[1]
  kd %>% filter(.data[[ic]] == "Kartal", abs(k_vpd - 0.5) < 1e-9) %>%
    as.data.frame() %>% print(row.names = FALSE)
  cat("Beklenen P_horizon: 0.9746 / 0.9922 / 0.9993\n")
} else cat("Dosya yok — kvpd_sensitivity_check_v3.R kosuldu mu?\n")

cat("\n=== KONTROL: PRCC p-degeri (0 gorunmemeli) ===\n")
pf <- file.path(CANON, "ssp245", "tables", "tbl_prcc.csv")
if (file.exists(pf)) print(as.data.frame(read_csv(pf, show_col_types = FALSE)),
                           row.names = FALSE)

cat("\n=== KONTROL: T_opt karsilastirmasi ('yansiz' gecmemeli) ===\n")
tf <- file.path(CANON, "tables", "topt_comparison.csv")
if (file.exists(tf)) print(as.data.frame(read_csv(tf, show_col_types = FALSE)),
                           row.names = FALSE)