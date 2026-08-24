# =============================================================================
# ek_a4_poisson_bernoulli_check.R                          (rev. 2026-08, v2)
# -----------------------------------------------------------------------------
# PURPOSE
#   Reproducibly regenerate Appendix Table A.4 of the thesis:
#   "Tumleyen carpiminin aylik nadir-olay yaklasimina duyarliligi".
#
#   For each district x SSP scenario the script compares
#       (a) the EXACT heterogeneous-Bernoulli complement product  (Eq. 12)
#       (b) the monthly rare-event / Poisson-hazard approximation (Eq. A.14)
#   and reports the second-order Taylor error bound Sum(p^2)/2.
#
# -----------------------------------------------------------------------------
# WHAT CHANGED IN v2  (read this before running)
#
#   [1] STALE DATA. v1 was last run against the pre-Poisson-thinning
#       (`ctmc_mc_rep1000`) tree, whose importation pressure was ~10x too low.
#       It produced Kartal SSP5-8.5 P_ufuk = 0.494. The canonical
#       Poisson-thinned value is 0.99927. All inputs must now come from the
#       corrected `ctmc_mc` tree (outputs/_canonical/ or outputs/<ssp>/).
#
#   [2] DISTRICT LABELS WERE SWAPPED. v1 mapped
#           TUR.59.4_1 -> "Zonguldak"        (WRONG)
#           TUR.81.6_1 -> "Fethiye (Mugla)"  (WRONG)
#       Canonical assignment is the reverse:
#           TUR.59.4_1 = Fethiye / Mugla     (Ae. albopictus)
#           TUR.81.6_1 = Zonguldak / Merkez  (Ae. aegypti)
#       Fixed below, and now asserted at runtime against the `species` column.
#
#   [3] THE TAYLOR ARGUMENT NO LONGER HOLDS FOR KARTAL. Under Poisson
#       thinning, Kartal has max p_ay ~ 0.18 and 25-59 months with p_ay > 0.05,
#       so Sum(p^2)/2 is O(1e-1), NOT negligible next to P_ufuk. The two
#       formulations still agree to < 0.3% relative, but that is because both
#       are in the SATURATION region, not because p is small. The script now
#       flags this explicitly instead of asserting rare-event validity.
#
#   [4] NEW COLUMN `p>0.10 ay`. v1 dropped it on the grounds that it was zero
#       everywhere. That is false post-Poisson (Kartal: 1 / 10 / 17 months).
#
#   [5] Added a `p_ay,major` identity check. Because
#           p_i = 1 - exp(-Lambda_i * P_est_i),
#       the complement product is ALGEBRAICALLY identical to
#           1 - exp(-sum_i Lambda_i * P_est_i).
#       This appendix therefore does NOT validate Poisson thinning (that is an
#       identity, not an approximation); it only probes the second, coarser
#       approximation taken at the p_ay level. The check below verifies the
#       identity numerically so the appendix text can state it safely.
#
# -----------------------------------------------------------------------------
# INPUTS  (first match wins, per scenario)
#   1. outputs/_canonical/<num>_ctmc_spark_monthly_2025_2075_rep1000.csv
#   2. outputs/<ssp>/simulation/ctmc_spark_monthly_2025_2075_rep1000.rds
#   3. outputs/<ssp>/simulation/ctmc_spark_monthly_2025_2075_rep1000.csv
#
#   Required columns: district_id, species, year, month,
#                     p_month_major_mean        (= p_ay,major)
#   Optional (enables the identity check in [5]):
#                     lambda_import, p_establishment_mean
#
# OUTPUTS
#   outputs/tables/tbl_ek_a4_poisson_bernoulli.csv       (machine-readable)
#   outputs/tables/tbl_ek_a4_poisson_bernoulli_docx.csv  (thesis formatting)
#   Console: reproduced table + prose self-check for Section 6.5.1.1.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(purrr)
  library(here)
})

# ---- Configuration ----------------------------------------------------------
SSP_SCENARIOS <- c("ssp126", "ssp245", "ssp585")

SSP_LABELS <- c(ssp126 = "SSP1-2.6",
                ssp245 = "SSP2-4.5",
                ssp585 = "SSP5-8.5")

SSP_NUM <- c(ssp126 = "126", ssp245 = "245", ssp585 = "585")

# CANONICAL district_id -> label / species. Do not edit without re-checking
# build_sentinel_species.R; the runtime assertion below depends on it.
DISTRICT_LABELS <- c(
  "TUR.40.25_1" = "Kartal (Istanbul)",
  "TUR.10.4_1"  = "Hopa (Artvin)",
  "TUR.59.4_1"  = "Fethiye (Mugla)",
  "TUR.81.6_1"  = "Zonguldak",
  "TUR.39.3_1"  = "Egirdir (Isparta)"
)

DISTRICT_SPECIES <- c(
  "TUR.40.25_1" = "albopictus",
  "TUR.10.4_1"  = "aegypti",
  "TUR.59.4_1"  = "albopictus",
  "TUR.81.6_1"  = "aegypti",
  "TUR.39.3_1"  = "albopictus"
)

# Display order within each scenario block (as printed in the thesis table).
DISTRICT_ORDER <- c("TUR.40.25_1", "TUR.10.4_1", "TUR.59.4_1",
                    "TUR.81.6_1", "TUR.39.3_1")

PROB_COL <- "p_month_major_mean"   # p_ay,major
N_MONTHS_EXPECTED <- 612L          # 51 years x 12 months

# ---- Path resolution --------------------------------------------------------
safe_here <- function(...) {
  rel <- file.path(...)
  tryCatch(here::here(rel), error = function(e) rel)
}

resolve_monthly <- function(ssp) {
  candidates <- c(
    safe_here("outputs", "_canonical",
              sprintf("%s_ctmc_spark_monthly_2025_2075_rep1000.csv", SSP_NUM[[ssp]])),
    safe_here("outputs", ssp, "simulation",
              "ctmc_spark_monthly_2025_2075_rep1000.rds"),
    safe_here("outputs", ssp, "simulation",
              "ctmc_spark_monthly_2025_2075_rep1000.csv")
  )
  hit <- candidates[file.exists(candidates)]
  if (length(hit) == 0) return(NA_character_)
  hit[[1]]
}

read_monthly <- function(path) {
  if (grepl("\\.rds$", path, ignore.case = TRUE)) {
    readRDS(path)
  } else {
    readr::read_csv(path, show_col_types = FALSE, progress = FALSE)
  }
}

# ---- Validation -------------------------------------------------------------
# Guards against (a) reading a stale pre-Poisson file and (b) a recurrence of
# the v1 district-label swap.
validate_monthly <- function(df, ssp, path) {

  need <- c("district_id", PROB_COL)
  missing <- setdiff(need, names(df))
  if (length(missing)) {
    stop(sprintf("[%s] missing column(s): %s  (%s)",
                 ssp, paste(missing, collapse = ", "), path))
  }

  ids <- sort(unique(df$district_id))
  if (!setequal(ids, names(DISTRICT_LABELS))) {
    stop(sprintf("[%s] unexpected district_id set: %s",
                 ssp, paste(ids, collapse = ", ")))
  }

  # (b) label-swap guard: species in the file must match the canonical mapping.
  if ("species" %in% names(df)) {
    obs <- df %>%
      distinct(district_id, species) %>%
      arrange(district_id)
    exp_sp <- unname(DISTRICT_SPECIES[obs$district_id])
    bad <- which(obs$species != exp_sp)
    if (length(bad)) {
      stop(sprintf(
        "[%s] district/species mismatch -> label mapping is wrong for: %s",
        ssp,
        paste(sprintf("%s (file=%s, expected=%s)",
                      obs$district_id[bad], obs$species[bad], exp_sp[bad]),
              collapse = "; ")))
    }
  } else {
    warning(sprintf("[%s] no `species` column; label-swap guard skipped.", ssp))
  }

  n_by_d <- df %>% count(district_id)
  if (any(n_by_d$n != N_MONTHS_EXPECTED)) {
    warning(sprintf("[%s] expected %d months per district, got: %s",
                    ssp, N_MONTHS_EXPECTED,
                    paste(n_by_d$n, collapse = "/")))
  }

  invisible(TRUE)
}

# ---- Core computation -------------------------------------------------------
compare_one_scenario <- function(monthly_df, ssp) {

  monthly_df %>%
    mutate(p = pmin(pmax(.data[[PROB_COL]], 0), 1)) %>%
    filter(!is.na(p)) %>%
    group_by(district_id) %>%
    summarise(
      n_months   = dplyr::n(),
      max_p      = max(p),
      n_p_gt_001 = sum(p > 0.01),
      n_p_gt_005 = sum(p > 0.05),
      n_p_gt_010 = sum(p > 0.10),
      P_exact    = 1 - prod(1 - p),   # Eq. 12
      Lambda_p   = sum(p),
      Sp2_over_2 = sum(p^2) / 2,      # 2nd-order Taylor bound
      .groups = "drop"
    ) %>%
    mutate(
      P_poisson    = 1 - exp(-Lambda_p),                    # Eq. A.14
      rel_diff_pct = dplyr::if_else(P_exact > 0,
                                    100 * abs(P_exact - P_poisson) / P_exact,
                                    NA_real_),
      # Is the rare-event justification usable for this cell?
      taylor_valid = Sp2_over_2 < 0.01 * P_exact,
      ssp            = ssp,
      ssp_label      = unname(SSP_LABELS[ssp]),
      district_label = unname(DISTRICT_LABELS[district_id])
    )
}

# Identity check [5]: 1 - prod(1 - p_i) == 1 - exp(-sum_i Lambda_i * P_est_i)
identity_check <- function(monthly_df, ssp) {
  cols <- c("lambda_import", "p_establishment_mean")
  if (!all(cols %in% names(monthly_df))) return(NULL)
  monthly_df %>%
    mutate(p = pmin(pmax(.data[[PROB_COL]], 0), 1)) %>%
    group_by(district_id) %>%
    summarise(
      P_complement = 1 - prod(1 - p),
      P_thinning   = 1 - exp(-sum(lambda_import * p_establishment_mean)),
      .groups = "drop"
    ) %>%
    mutate(abs_dev = abs(P_complement - P_thinning),
           ssp = ssp)
}

# ---- Run --------------------------------------------------------------------
message("Reading monthly simulation outputs and computing Table A.4 ...")

loaded <- purrr::map(SSP_SCENARIOS, function(ssp) {
  path <- resolve_monthly(ssp)
  if (is.na(path)) {
    warning(sprintf("[%s] no monthly file found; scenario skipped.", ssp))
    return(NULL)
  }
  df <- read_monthly(path)
  message(sprintf("  [%s] %-58s rows=%d", ssp, basename(path), nrow(df)))
  validate_monthly(df, ssp, path)
  list(ssp = ssp, df = df)
})
loaded <- purrr::compact(loaded)

if (length(loaded) == 0) {
  stop("No scenario files could be read. Check outputs/_canonical/ and outputs/<ssp>/simulation/.")
}

results <- purrr::map_dfr(loaded, ~ compare_one_scenario(.x$df, .x$ssp))
ident   <- purrr::map_dfr(loaded, ~ identity_check(.x$df, .x$ssp))

results <- results %>%
  mutate(ssp         = factor(ssp, levels = SSP_SCENARIOS),
         district_id = factor(district_id, levels = DISTRICT_ORDER)) %>%
  arrange(ssp, district_id) %>%
  mutate(ssp = as.character(ssp), district_id = as.character(district_id))

# ---- Machine-readable output ------------------------------------------------
out_dir <- safe_here("outputs", "tables")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

readr::write_csv(
  results %>% select(ssp, ssp_label, district_id, district_label, n_months,
                     max_p, n_p_gt_001, n_p_gt_005, n_p_gt_010,
                     P_exact, P_poisson, rel_diff_pct, Lambda_p,
                     Sp2_over_2, taylor_valid),
  file.path(out_dir, "tbl_ek_a4_poisson_bernoulli.csv")
)

# ---- Thesis-formatted output (Turkish decimal comma, 3 sig. digits) ---------
tr_sci <- function(x, digits = 3) {
  out <- formatC(x, format = "e", digits = digits - 1)
  out <- sub("e([+-])0*(\\d+)", " x 10^\\1\\2", out)
  gsub("\\.", ",", out)
}
tr_fix <- function(x, digits) gsub("\\.", ",", formatC(x, format = "f", digits = digits))

tbl_docx <- results %>%
  transmute(
    Senaryo              = ssp_label,
    `Ilce`               = district_label,
    `maks p_ay`          = tr_sci(max_p),
    `p>0,01 ay`          = n_p_gt_001,
    `p>0,05 ay`          = n_p_gt_005,
    `p>0,10 ay`          = n_p_gt_010,
    `P_ufuk (kesin)`     = tr_sci(P_exact, 4),
    `P_ufuk (yaklasik)`  = tr_sci(P_poisson, 4),
    `Bagil fark (%)`     = tr_fix(rel_diff_pct, 3),
    `Sum p^2 / 2`        = tr_sci(Sp2_over_2)
  )
readr::write_csv(tbl_docx, file.path(out_dir, "tbl_ek_a4_poisson_bernoulli_docx.csv"))

message(sprintf("\nTables written to: %s", out_dir))

# ---- Console report ---------------------------------------------------------
cat("\n================ APPENDIX TABLE A.4 (reproduced) ================\n")
print(as.data.frame(
  results %>% transmute(
    Senaryo = ssp_label,
    Ilce    = district_label,
    max_p   = formatC(max_p, format = "e", digits = 2),
    `p>.01` = n_p_gt_001,
    `p>.05` = n_p_gt_005,
    `p>.10` = n_p_gt_010,
    P_exact = formatC(P_exact, format = "e", digits = 3),
    P_appr  = formatC(P_poisson, format = "e", digits = 3),
    `rel%`  = formatC(rel_diff_pct, format = "f", digits = 3),
    `Sp2/2` = formatC(Sp2_over_2, format = "e", digits = 2)
  )), row.names = FALSE)

worst <- results %>% filter(!is.na(rel_diff_pct)) %>% arrange(desc(rel_diff_pct)) %>% slice(1)
cat(sprintf("\nMaksimum bagil fark: %%%.3f  (%s, %s)\n",
            worst$rel_diff_pct, worst$district_label, worst$ssp_label))

# ---- [5] identity check -----------------------------------------------------
cat("\n---- Poisson inceltme ozdesligi (tumleyen carpimi == 1 - exp(-sum Lambda*P_est)) ----\n")
# NOTE ON TOLERANCE: the identity is exact for a single realisation, but the
# reported columns are OUTER-MC MEANS. p_month_major_mean averages
# 1 - exp(-Lambda * P_est_rep) over reps, whereas the right-hand side uses
# mean(P_est). The residual is therefore the Jensen gap of the outer loop,
# not a modelling error; it is O(1e-7) at n_rep = 1000. A residual larger
# than IDENTITY_TOL means the p_ay column was NOT produced by Poisson thinning.
IDENTITY_TOL <- 1e-5
if (nrow(ident) > 0) {
  max_dev <- max(ident$abs_dev, na.rm = TRUE)
  cat(sprintf("  maks |fark| = %.3e   (dis-MC Jensen artigi; beklenen ~1e-7)\n", max_dev))
  if (max_dev > IDENTITY_TOL) {
    cat("  [UYARI] Ozdeslik saglanmiyor: p_ay sutunu Poisson inceltmesiyle\n")
    cat("          uretilmemis olabilir (Bernoulli-donemi dosya?).\n")
  } else {
    cat("  Ozdeslik saglaniyor: tumleyen carpimi = Poisson inceltmesi (Denklem 12 == 10).\n")
  }
} else {
  cat("  [BILGI] lambda_import / p_establishment_mean sutunlari yok; atlandi.\n")
}

# ---- [3] rare-event justification audit ------------------------------------
cat("\n---- Nadir-olay (Taylor) gerekcesi denetimi ----\n")
bad <- results %>% filter(!taylor_valid)
if (nrow(bad) == 0) {
  cat("  Tum hucrelerde Sum(p^2)/2 < %1 x P_ufuk: nadir-olay gerekcesi gecerli.\n")
} else {
  cat("  [DIKKAT] Asagidaki hucrelerde Sum(p^2)/2 ihmal edilebilir DEGILDIR;\n")
  cat("           metinde 'p_ay kucuk oldugu icin esdeger' denemez. Dogru gerekce:\n")
  cat("           her iki hesap da doygunluk bolgesindedir.\n")
  print(as.data.frame(bad %>% transmute(
    Senaryo = ssp_label, Ilce = district_label,
    max_p   = formatC(max_p, format = "e", digits = 2),
    `Sp2/2` = formatC(Sp2_over_2, format = "e", digits = 2),
    P_ufuk  = formatC(P_exact, format = "f", digits = 5),
    `rel%`  = formatC(rel_diff_pct, format = "f", digits = 3)
  )), row.names = FALSE)
}

# ---- Prose self-check for Section 6.5.1.1 ----------------------------------
# Canonical values the prose MUST report (post-Poisson, n_rep = 1000):
#   Kartal SSP5-8.5 : exact 0.99927, approx 0.99898, rel 0.029%
#   Kartal SSP1-2.6 : rel 0.280%  <- largest relative difference overall
EXPECTED <- tibble::tribble(
  ~ssp,     ~district_id,   ~P_exact, ~P_poisson, ~rel_diff_pct,
  "ssp585", "TUR.40.25_1",  0.99927,  0.99898,    0.029,
  "ssp126", "TUR.40.25_1",  0.97457,  0.97185,    0.280
)

cat("\n---- Bolum 6.5.1.1 metni ozdenetimi ----\n")
chk <- EXPECTED %>%
  left_join(results %>% select(ssp, district_id, obs_exact = P_exact,
                               obs_pois = P_poisson, obs_rel = rel_diff_pct),
            by = c("ssp", "district_id"))

for (i in seq_len(nrow(chk))) {
  r <- chk[i, ]
  if (is.na(r$obs_exact)) {
    cat(sprintf("  [BILGI] %s / %s satiri yok.\n", r$ssp, r$district_id))
    next
  }
  ok <- abs(r$obs_exact - r$P_exact) < 5e-4 &&
        abs(r$obs_rel   - r$rel_diff_pct) < 5e-3
  cat(sprintf("  %s %s: kesin=%.5f  yaklasik=%.5f  bagil=%%%.3f  [%s]\n",
              r$ssp, r$district_id, r$obs_exact, r$obs_pois, r$obs_rel,
              if (ok) "OK" else "SAPMA"))
  if (!ok) {
    cat("     [UYARI] Kanonik degerden sapma. Ya girdi dosyasi bayat (Bernoulli\n")
    cat("             donemi ctmc_mc_rep1000 agaci) ya da metin guncellenmeli.\n")
  }
}

# Explicit stale-file tripwire: the pre-Poisson run gave Kartal ssp585 ~ 0.494.
k585 <- results %>% filter(district_id == "TUR.40.25_1", ssp == "ssp585")
if (nrow(k585) == 1 && abs(k585$P_exact - 0.494) < 0.02) {
  stop("BAYAT GIRDI: Kartal SSP5-8.5 icin P_ufuk ~ 0,494 bulundu. Bu deger\n",
       "  Poisson inceltme oncesi (ctmc_mc_rep1000) agacina aittir.\n",
       "  Duzeltilmis ctmc_mc / outputs/_canonical agacindan yeniden calistirin.")
}

cat("\nBitti.\n")
