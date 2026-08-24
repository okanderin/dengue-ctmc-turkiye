# =============================================================================
# 01_audit_duplicates.R                                        [ADIM 1 - OKU]
# Amac : Kanonik ciktilarin coklu kopyalarini tarar; her birini (a) MC mi
#        ANALYTIC mi, (b) Poisson mu Bernoulli mi olarak etiketler.
#        HICBIR SEY silinmez / tasinmaz.
# Cikti: outputs/_audit/duplicate_audit.csv
# =============================================================================

library(here); library(dplyr); library(readr)
library(digest); library(purrr); library(tidyr)

SSPS      <- c("ssp126", "ssp245", "ssp585")
KARTAL_ID <- "TUR.40.25_1"
AUDIT_DIR <- here("outputs", "_audit")
dir.create(AUDIT_DIR, recursive = TRUE, showWarnings = FALSE)

# ONEMLI: klasor adin gercekten "Yeni klasör" (U+00F6) ise burayi degistir.
YENI <- "Yeni_klasor"

candidate_paths <- function(ssp) {
  n <- sub("ssp", "", ssp)
  c(
    # ---- Monte Carlo ciktilari (ctmc_spark_monte_carlo.R) ----
    MC_A_simulation    = here("outputs", ssp, "simulation",
                              "ctmc_spark_horizon_2025_2075_rep1000.csv"),
    MC_B_frozen_sim    = here("outputs", "frozen", ssp, "simulation",
                              "ctmc_spark_horizon_2025_2075_rep1000.csv"),
    MC_C_sim_yeni      = here("outputs", ssp, "simulation", YENI,
                              paste0(ssp, "_ctmc_spark_horizon_2025_2075_rep1000.csv")),
    MC_D_root_yeni     = here("outputs", YENI,
                              paste0(n, "_ctmc_spark_horizon_2025_2075_rep1000.csv")),
    MC_E_shiny         = here("shiny_dengue_app", "outputs", ssp, "simulation",
                              "ctmc_spark_horizon_2025_2075_rep1000.csv"),
    MC_F_sens_base     = here("outputs", ssp, "sensitivity", "ctmc_mc", "base",
                              "ctmc_spark_horizon_2025_2075_rep1000.csv"),
    MC_G_oat_m100      = here("outputs", ssp, "sensitivity", "ctmc_mc_rep1000", "m_100",
                              "ctmc_spark_horizon_2025_2075_rep1000.csv"),
    # ---- Analitik ciktilar (ctmc_spark.R) — MUKERRER DEGIL, AYRI URUN ----
    AN_A_model_results = here("outputs", ssp, "model_results",
                              "ctmc_spark_horizon_2025_2075.csv"),
    AN_B_frozen_model  = here("outputs", "frozen", ssp, "model_results",
                              "ctmc_spark_horizon_2025_2075.csv")
  )
}

probe_file <- function(path) {
  if (!file.exists(path))
    return(tibble(exists = FALSE, sha256 = NA_character_, n_row = NA_integer_,
                  kartal_p = NA_real_, kind = NA_character_, col_used = NA_character_,
                  mtime = as.POSIXct(NA), verdict = "MISSING"))
  
  sh <- digest(file = path, algo = "sha256")
  mt <- file.info(path)$mtime
  d  <- tryCatch(read_csv(path, show_col_types = FALSE), error = function(e) NULL)
  if (is.null(d))
    return(tibble(exists = TRUE, sha256 = sh, n_row = NA_integer_, kartal_p = NA_real_,
                  kind = NA_character_, col_used = NA_character_,
                  mtime = mt, verdict = "UNREADABLE"))
  
  # MC ciktisi *_mean/_p2_5/_p97_5 tasir; analitik cikti tasimaz.
  is_mc  <- "p_ge1_major_mean" %in% names(d)
  is_ana <- !is_mc && "p_ge1_major" %in% names(d)
  col    <- if (is_mc) "p_ge1_major_mean" else if (is_ana) "p_ge1_major" else NA_character_
  kind   <- if (is_mc) "MC" else if (is_ana) "ANALYTIC" else "UNKNOWN"
  
  if (is.na(col))
    return(tibble(exists = TRUE, sha256 = sh, n_row = nrow(d), kartal_p = NA_real_,
                  kind = kind, col_used = NA_character_,
                  mtime = mt, verdict = "SCHEMA MISMATCH"))
  
  kp <- d[[col]][d$district_id == KARTAL_ID]
  kp <- if (length(kp) == 1) kp else NA_real_
  verdict <- dplyr::case_when(
    is.na(kp) ~ "NO KARTAL ROW",
    kp > 0.90 ~ "POISSON (post-correction)",
    kp < 0.60 ~ "BERNOULLI (SUPERSEDED)",
    TRUE      ~ "UNKNOWN RANGE"
  )
  tibble(exists = TRUE, sha256 = sh, n_row = nrow(d), kartal_p = kp,
         kind = kind, col_used = col, mtime = mt, verdict = verdict)
}

audit <- map_dfr(SSPS, function(ssp) {
  p <- candidate_paths(ssp)
  map_dfr(seq_along(p), function(i) {
    probe_file(p[[i]]) %>%
      mutate(ssp = ssp, slot = names(p)[i], path = p[[i]], .before = 1)
  })
})

write_csv(audit, file.path(AUDIT_DIR, "duplicate_audit.csv"))

cat("\n=== 1) DOSYA ENVANTERI ===\n")
audit %>% select(ssp, slot, kind, verdict, kartal_p, n_row, mtime) %>%
  arrange(ssp, slot) %>% as.data.frame() %>% print(row.names = FALSE)

cat("\n=== 2) HASH ESDEGERLIK SINIFLARI (tur bazinda) ===\n")
audit %>% filter(exists, !is.na(sha256)) %>%
  group_by(ssp, kind, sha256) %>%
  summarise(n = n(), slots = paste(slot, collapse = ", "), .groups = "drop") %>%
  as.data.frame() %>% print(row.names = FALSE)

cat("\n=== 3) ANALITIK vs MC UYUMU (Kartal P_ufuk) ===\n")
cmp <- audit %>% filter(!is.na(kartal_p), kind %in% c("MC", "ANALYTIC")) %>%
  group_by(ssp, kind) %>% summarise(kartal_p = first(kartal_p), .groups = "drop") %>%
  pivot_wider(names_from = kind, values_from = kartal_p)
if (all(c("MC", "ANALYTIC") %in% names(cmp)))
  cmp <- cmp %>% mutate(abs_diff = abs(MC - ANALYTIC),
                        flag = ifelse(abs_diff > 0.02, "!! AYRISMA", "ok"))
as.data.frame(cmp) %>% print(row.names = FALSE)

cat("\n=== 4) UYARILAR ===\n")
bad <- audit %>% filter(verdict == "BERNOULLI (SUPERSEDED)")
if (nrow(bad) > 0) {
  cat("!! Bernoulli donemi dosyalar (dondurmadan ONCE arsivle):\n")
  cat(paste0("   [", bad$kind, "] ", bad$path, collapse = "\n"), "\n")
} else cat("Bernoulli donemi dosya bulunamadi.\n")

if (nrow(audit %>% filter(verdict == "MISSING", grepl("^AN_A", slot))) > 0)
  cat("\nNot: outputs/{ssp}/model_results/ yok; analitik kaynak olarak\n",
      "     outputs/frozen/{ssp}/model_results/ kullan (betik 02, ANALYTIC_SRC).\n", sep = "")

cat("\nYazildi: ", file.path(AUDIT_DIR, "duplicate_audit.csv"), "\n", sep = "")