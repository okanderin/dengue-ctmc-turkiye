## =========================================================
## R/04_results/figures_en.R                       (rev. 2026-08, v2-EN)
## English-labeled twin of R/04_results/core/01_generate_ssp_outputs.R,
## prepared for the PLOS Neglected Tropical Diseases submission and its
## reproducibility package.
##
## WHAT THIS FILE IS
##   A translation of the DISPLAY TEXT in 01_generate_ssp_outputs.R --
##   plot titles/subtitles, axis labels, legend titles and level labels,
##   facet strip labels, in-plot annotations, district-name spelling,
##   table column headers, and code comments. ALL data processing --
##   filters, joins, thresholds (LAMBDA_THRESH = 1e-4, NOISE_FLOOR =
##   1e-15, gamma_val = 1/5, TAU_VAL = 30), colour scales, statistical
##   computations, and ggsave() dimensions/DPI -- is UNCHANGED from the
##   source file. This is a translation, not a re-analysis or a bug
##   fix: it carries forward whatever the v2 code in 01_generate_ssp_
##   outputs.R actually computes, unchanged. If that file's computations
##   are ever revised, this file must be re-synced by hand.
##
## USAGE (identical pattern to the original)
##   Sys.setenv(SSP_SCENARIO = "ssp245")
##   source("R/04_results/figures_en.R")
##
##   or, for all three SSPs:
##   for (s in c("ssp126", "ssp245", "ssp585")) {
##     Sys.setenv(SSP_SCENARIO = s)
##     source("R/04_results/figures_en.R")
##   }
##
## OUTPUTS
##   Figures : outputs/{ssp}/figures_en/   (same base filenames as the
##             Turkish originals in outputs/{ssp}/figures/, e.g.
##             fig_temp_rh.png -- so nothing already produced by
##             01_generate_ssp_outputs.R is ever overwritten)
##   Tables  : outputs/{ssp}/tables_en/    (English column headers over
##             the same rows/values as outputs/{ssp}/tables/; this
##             parallel table directory is a choice made for this
##             translation, since the task brief specified only the
##             figures_en/ pattern explicitly -- flagged for review)
##
## OUT OF SCOPE -- NOT PRODUCED BY THIS SCRIPT
##   Two conceptual/schematic figures referenced in the manuscript are
##   hand-drawn (BioRender-style) diagrams, not outputs of this data
##   pipeline, and neither this script nor the Turkish original
##   generates them:
##     (1) study-framework overview diagram
##     (2) CTMC state-transition (spark-phase birth-death) diagram
##   These must be created separately by the authors (e.g. in
##   BioRender or a vector-drawing tool) and inserted into the
##   manuscript directly.
##
## DEPENDENCY NOTE
##   00_results_setup.R (defines SSP_LABELS, here(), theme_thesis(),
##   COL_DISTRICT, COL_DISTRICT_LABEL, DISTRICT_LABELS, AY_TR, SCEN_TR,
##   load_ssp_data(), load_sens_imp(), load_sens_mc(),
##   load_country_contrib(), and the tidyverse/flextable/officer/scales
##   library() calls) was NOT present among the files made available
##   for this translation, so its internals could not be read or
##   translated. Per instructions, it is left as infrastructure: it is
##   sourced below exactly as the original does, unmodified (it may
##   stay in Turkish/mixed -- it is not reader-facing text). English
##   display-label vectors/helpers that would normally live in a
##   translated setup file are instead defined locally right after the
##   source() call, following the pattern already established in
##   R/04_results/_archive/fig_oat_m_profile_en.Rmd
##   (DISTRICT_LABELS_EN <- gsub("Istanbul", ...)). See the "ENGLISH
##   DISPLAY-LABEL OVERRIDES" block below.
##
## FLAGGED FOR REVIEW (see final task report for the full list)
##   - SSP_LABELS_EN, MONTH_TR_EN and the tornado scenario-label
##     translation are best-effort reconstructions made without sight
##     of 00_results_setup.R's actual string contents. Verify against
##     the real SSP_LABELS / AY_TR / SCEN_TR values before publication.
## =========================================================

## ---------------------------------------------------------
## v2 CHANGELOG (translated from the Turkish original; carried forward
## unchanged in substance -- read before running)
##
## [1] PROVENANCE AUDIT ADDED (Section 0).
##     load_ssp_data() selects files by file.mtime(), and its regex
##     ("ctmc_spark_monthly.*rep.*") also matches outputs from the
##     earlier Bernoulli-period pipeline. mtime is not reliable inside
##     a cloud-synced folder. Also, monthly/yearly/horizon are selected
##     INDEPENDENTLY and so can come from different runs. Four
##     mandatory checks now run after loading; the script stops on
##     failure.
##
## [2] SILENT DATA LOSS ON LOG-SCALED FIGURES FIXED.
##     scale_y_log10 drops zero and negative values without warning.
##     In SSP5-8.5 / Hopa / 2028, p_ge1_major_year_mean is EXACTLY 0
##     (catastrophic cancellation of 1-exp(-x) in the pipeline;
##     x ~ 1.7e-18). In addition, every value below ~1.1e-16 has been
##     quantised to multiples of machine epsilon -- numerical noise,
##     not signal. A NOISE_FLOOR constant was added and the number of
##     censored rows is reported. The PERMANENT FIX lives in the
##     pipeline: -expm1(-x) instead of 1-exp(-x).
##
## [3] THE LHS-PRCC OUTPUT IS R0, NOT P_horizon (Section 10).
##     compute_R0_lhs() computes R0 and PRCC is taken against R0. Any
##     "P_horizon" wording in the thesis text CONTRADICTS THE CODE. See
##     the warning block at the start of Section 10.
##
## [4] THE PRCC CONFIDENCE INTERVAL IS NOW A GENUINE BOOTSTRAP.
##     v1 used cor.test()$conf.int (Fisher z); the thesis text says
##     "bootstrap confidence interval". Point estimates are unchanged.
##
## [5] IN SECTION 15, T_opt IS NOW COMPUTED FROM M2.
##     v1 derived T_opt from M3, which includes Lambda_import; that
##     produces the "OLS ~19 C" collinearity artefact the thesis
##     explicitly treats as invalid.
##
## [6] SECTIONS 15 AND 16 OUTPUT FILE NAMES WERE CHANGED.
##     These sections do NOT produce the thesis's Section 6.10
##     regression tables; they use a different dependent variable. The
##     canonical source is 6_10_pest_regression.R. The filename clash
##     was therefore removed.
##
## [7] SECTION 16 RAN SILENTLY.
##     Top-level expressions inside source() are not auto-printed;
##     the anova(), tidy(), VarCorr() lines produced no output at all.
##     Wrapped in print(), results are now written to disk, and the
##     section was moved ABOVE the DONE banner.
##
## [8] SECTION 14 AXIS LABEL FIXED.
##     The label read "lambda_local > 0" while the computation actually
##     used LAMBDA_THRESH = 1e-4 (which matches the thesis definition).
##
## [9] Section 6 "Model validation" renamed to "Stage 2: predictor
##     comparison" (Stage 1 requires a sigma_EIP = 0 run and is not
##     performed in this script).
## ---------------------------------------------------------

source("R/04_results/00_results_setup.R")

SSP <- Sys.getenv("SSP_SCENARIO", unset = "ssp245")
SSP_LABEL <- SSP_LABELS[SSP]     # kept for parity with the original / any internal comparisons

## =========================================================
## ENGLISH DISPLAY-LABEL OVERRIDES
## (local to this file; none of these touch the Turkish objects
## defined in 00_results_setup.R, and none of them change any join
## key, filter, or computed value -- see DEPENDENCY NOTE above)
## =========================================================

## ---- SSP scenario labels ----
## Best-effort reconstruction -- FLAG FOR REVIEW against the real
## SSP_LABELS strings in 00_results_setup.R.
SSP_LABELS_EN <- c(
  ssp126 = "SSP1-2.6 (low emissions)",
  ssp245 = "SSP2-4.5 (intermediate emissions)",
  ssp585 = "SSP5-8.5 (high emissions)"
)
SSP_LABEL_EN <- unname(SSP_LABELS_EN[SSP])
if (is.na(SSP_LABEL_EN)) SSP_LABEL_EN <- as.character(SSP_LABEL)  # fallback, unlikely to trigger

## ---- District display labels: strip Turkish diacritics ----
## Same pattern as fig_oat_m_profile_en.Rmd's
## DISTRICT_LABELS_EN <- gsub("Istanbul", ...), extended to the other
## two diacritic-bearing province names that appear in this pipeline's
## district labels (Egirdir/Isparta, Fethiye/Mugla). district_id and
## district_label VALUES in the data are never touched -- only these
## display vectors / label functions are.
translate_district_en <- function(x) {
  x <- gsub("İstanbul", "Istanbul", x)   # Istanbul -> Istanbul
  x <- gsub("Muğla",   "Mugla",    x)    # Mugla -> Mugla
  x <- gsub("Eğirdir", "Egirdir",  x)    # Egirdir -> Egirdir
  x
}
DISTRICT_LABELS_EN <- translate_district_en(DISTRICT_LABELS)

## ---- Month display labels ----
## FLAG FOR REVIEW: the exact strings used for AY_TR / monthly$month_name
## in 00_results_setup.R were not available when this file was written.
## The lookup below covers both common Turkish representations (3-letter
## abbreviation and full name); anything unmatched passes through
## unchanged. Verify against the real AY_TR contents before publication.
MONTH_TR_EN <- c(
  "Oca" = "Jan", "Şub" = "Feb", "Mar" = "Mar", "Nis" = "Apr",
  "May" = "May", "Haz" = "Jun", "Tem" = "Jul", "Ağu" = "Aug",
  "Eyl" = "Sep", "Eki" = "Oct", "Kas" = "Nov", "Ara" = "Dec",
  "Ocak" = "January", "Şubat" = "February", "Mart" = "March",
  "Nisan" = "April", "Mayıs" = "May", "Haziran" = "June",
  "Temmuz" = "July", "Ağustos" = "August", "Eylül" = "September",
  "Ekim" = "October", "Kasım" = "November", "Aralık" = "December"
)
translate_month_en <- function(x) {
  out <- unname(MONTH_TR_EN[as.character(x)])
  ifelse(is.na(out), as.character(x), out)
}
AY_EN <- if (exists("AY_TR")) translate_month_en(AY_TR) else NULL

## ---- LHS-PRCC / sensitivity parameter display labels ----
## Follows the naming convention already used in
## R/04_results/_archive/LHS_PRCC_EN_ref_1-1.R.
PARAM_LABELS_EN <- c(
  T_C     = "Temperature (T_C)",
  RH      = "Relative humidity (RH)",
  m       = "Mosquito:human ratio (m)",
  beta_vh = "β_vh",
  beta_hv = "β_hv",
  ip_days = "Infectious period (ip_days)"
)

## ---- Best-effort Turkish -> English gloss for free-text scenario
## labels coming out of SCEN_TR (Section 8, tornado plot). SCEN_TR's
## contents were not available for inspection; this is a generic
## word-substitution fallback, NOT a verified translation. FLAG FOR
## REVIEW: check fig_tornado.png's bar labels against the underlying
## sensitivity_tornado.csv$scenario codes before publication.
translate_scenario_label_en <- function(x) {
  repl <- c(
    "sivrisinek.insan" = "mosquito:human", "sivrisinek" = "mosquito",
    "bulaşıcı dönem" = "infectious period",
    "sıcaklık" = "temperature", "nem" = "humidity",
    "artış" = "increase", "azalış" = "decrease",
    "yüksek" = "high", "düşük" = "low",
    "temel" = "baseline", "taban" = "baseline",
    "oranı" = "ratio", "senaryo" = "scenario"
  )
  for (tr in names(repl)) x <- gsub(tr, repl[[tr]], x, ignore.case = TRUE)
  x
}

cat("\n", strrep("=", 55), "\n")
cat("Generating outputs for:", SSP_LABEL_EN, "\n")
cat(strrep("=", 55), "\n\n")

# ---- Output directories (parallel to the Turkish originals) ----
DIR_FIG   <- here("outputs", SSP, "figures_en")
DIR_TBL   <- here("outputs", SSP, "tables_en")
dir.create(DIR_FIG, recursive = TRUE, showWarnings = FALSE)
dir.create(DIR_TBL, recursive = TRUE, showWarnings = FALSE)

# ---- Load data (identical to the original) ----
dat <- load_ssp_data(SSP)
monthly <- dat$monthly
yearly  <- dat$yearly
horizon <- dat$horizon

gamma_val <- 1 / 5      # day^-1  (infectious period = 5 days; thesis baseline)
TAU_VAL   <- 30L

## Numerical noise floor. In double precision, 1-exp(-x) returns
## exactly 0 for x < ~1e-16; every value below this threshold is a
## quantisation artefact at multiples of eps/2 = 1.11e-16.
NOISE_FLOOR <- 1e-15

cat("Data loaded:\n")
cat("  Monthly rows:", nrow(monthly), "\n")
cat("  Year range:", range(monthly$year), "\n")
cat("  Districts:", nlevels(monthly$district_id), "\n\n")


## =========================================================
## 0) PROVENANCE VALIDATION  (added in v2)
## ---------------------------------------------------------
## Purpose: because load_ssp_data() picks files by mtime, prevent
## accidentally reading from the earlier Bernoulli-period (pre-Poisson-
## thinning) output tree, and prevent monthly/yearly/horizon coming
## from different runs.
##
## Four checks:
##   D1  Poisson-thinning identity:
##         p_month,major == -expm1(-lambda_import * p_establishment)
##       Bernoulli-period files fail this (deviation ~1e-1).
##   D2  Can yearly be rebuilt from monthly?
##   D3  Can horizon be rebuilt from yearly?
##   D4  Are the Lambda totals consistent?
## =========================================================
cat(">>> 0) Provenance validation <<<\n")

TOL_IDENT   <- 1e-5    # cross-MC Jensen residual: observed <= 1e-6
TOL_REBUILD <- 1e-5
TOL_LAMBDA  <- 1e-9

.fail <- function(...) stop("[PROVENANCE] ", ..., call. = FALSE)

# --- D1: Poisson-thinning identity ---
dev_poisson <- with(monthly,
  max(abs(p_month_major_mean - (-expm1(-lambda_import * p_establishment_mean))),
      na.rm = TRUE))

# Deviation under the Bernoulli construction, for comparison (diagnostic only)
dev_bernoulli <- if ("q_import_month" %in% names(monthly)) {
  with(monthly, max(abs(p_month_major_mean - q_import_month * p_establishment_mean),
                    na.rm = TRUE))
} else NA_real_

cat(sprintf("  D1 Poisson-thinning identity : deviation = %.3e (tolerance %.0e)\n",
            dev_poisson, TOL_IDENT))
if (!is.na(dev_bernoulli))
  cat(sprintf("     (comparison) Bernoulli deviation = %.3e\n", dev_bernoulli))

if (dev_poisson > TOL_IDENT) {
  .fail("The p_month,major column does not appear to have been produced by Poisson thinning.\n",
        "  Likely cause: a Bernoulli-period (ctmc_mc_rep1000) file exists inside\n",
        "  outputs/", SSP, "/simulation/ and file.mtime() picked it.\n",
        "  Fix: move the old files out of simulation/, or pin load_ssp_data()\n",
        "  to a specific filename.")
}

# --- D2: yearly <- monthly ---
yr_rebuilt <- monthly %>%
  group_by(district_id, year) %>%
  summarise(rec = 1 - prod(1 - p_month_major_mean), .groups = "drop")

dev_yearly <- yearly %>%
  select(district_id, year, p_ge1_major_year_mean) %>%
  inner_join(yr_rebuilt, by = c("district_id", "year")) %>%
  summarise(d = max(abs(p_ge1_major_year_mean - rec), na.rm = TRUE)) %>%
  pull(d)

cat(sprintf("  D2 yearly <- monthly          : deviation = %.3e\n", dev_yearly))
if (!is.finite(dev_yearly) || dev_yearly > TOL_REBUILD)
  .fail("The yearly file cannot be rebuilt from monthly.\n",
        "  monthly and yearly may come from different runs.")

# --- D3: horizon <- yearly ---
hz_rebuilt <- yearly %>%
  group_by(district_id) %>%
  summarise(rec = 1 - prod(1 - p_ge1_major_year_mean), .groups = "drop")

dev_horizon <- horizon %>%
  select(district_id, p_ge1_major_mean) %>%
  inner_join(hz_rebuilt, by = "district_id") %>%
  summarise(d = max(abs(p_ge1_major_mean - rec), na.rm = TRUE)) %>%
  pull(d)

cat(sprintf("  D3 horizon <- yearly          : deviation = %.3e\n", dev_horizon))
if (!is.finite(dev_horizon) || dev_horizon > TOL_REBUILD)
  .fail("The horizon file cannot be rebuilt from yearly.")

# --- D4: Lambda consistency ---
dev_lambda <- yearly %>%
  group_by(district_id) %>%
  summarise(lam = sum(Lambda_import_year), .groups = "drop") %>%
  inner_join(select(horizon, district_id, Lambda_import), by = "district_id") %>%
  summarise(d = max(abs(lam - Lambda_import), na.rm = TRUE)) %>%
  pull(d)

cat(sprintf("  D4 Lambda consistency         : deviation = %.3e\n", dev_lambda))
if (!is.finite(dev_lambda) || dev_lambda > TOL_LAMBDA)
  .fail("Lambda_import totals are inconsistent between yearly and horizon.")

# --- Summary and scale diagnostic ---
lam_kartal <- horizon %>%
  filter(district_id == "TUR.40.25_1") %>% pull(Lambda_import)
if (length(lam_kartal) == 1) {
  cat(sprintf("  Kartal Lambda_horizon = %.1f\n", lam_kartal))
  if (lam_kartal < 100)
    warning("[PROVENANCE] Kartal Lambda_horizon < 100. The canonical post-Poisson\n",
            "  value lies in the 228-290 range. You may be reading from an old\n",
            "  tree where importation pressure was computed with a 3-day window.",
            call. = FALSE)
}

prov_tbl <- tibble(
  Check     = c("D1 Poisson-thinning identity", "D2 yearly<-monthly",
                "D3 horizon<-yearly", "D4 Lambda consistency",
                "Kartal Lambda_horizon"),
  Deviation = c(formatC(dev_poisson, format = "e", digits = 2),
                formatC(dev_yearly,  format = "e", digits = 2),
                formatC(dev_horizon, format = "e", digits = 2),
                formatC(dev_lambda,  format = "e", digits = 2),
                formatC(ifelse(length(lam_kartal) == 1, lam_kartal, NA), format = "f", digits = 1)),
  ssp       = SSP,
  timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
)
write_csv(prov_tbl, file.path(DIR_TBL, "tbl_provenance_check.csv"))
cat("  Provenance validation PASSED.\n\n")


## ---------------------------------------------------------
## Helper: noise-floor censoring for log-scaled figures.
## scale_y_log10 drops zero/negative values WITHOUT WARNING.
## This function censors first, then reports how many rows were dropped.
## ---------------------------------------------------------
censor_log <- function(df, col, floor_val = NOISE_FLOOR, label = col) {
  v <- df[[col]]
  n_zero  <- sum(v <= 0, na.rm = TRUE)
  n_noise <- sum(v > 0 & v < floor_val, na.rm = TRUE)
  if (n_zero + n_noise > 0) {
    cat(sprintf("    [log-censor] %s: %d zero, %d below-noise-floor (<%.0e) rows set to NA\n",
                label, n_zero, n_noise, floor_val))
  }
  df[[col]] <- ifelse(is.na(v) | v < floor_val, NA_real_, v)
  df
}


## =========================================================
## 1) Climate Conditions
## =========================================================
cat(">>> 1) Temperature-humidity profile <<<\n")

p_temp_rh <- monthly %>%
  group_by(district_id, district_label, month_name) %>%
  summarise(T_mean  = mean(temp_c, na.rm = TRUE),
            RH_mean = mean(rh,     na.rm = TRUE),
            .groups = "drop") %>%
  ggplot(aes(x = month_name, group = district_id, colour = district_id)) +
  geom_line(aes(y = T_mean, linetype = "Temperature (°C)"), linewidth = 0.8) +
  geom_point(aes(y = T_mean), size = 1.5) +
  geom_line(aes(y = RH_mean / 2.5, linetype = "Relative Humidity (%)"), alpha = 0.6) +
  scale_colour_manual(
    values = COL_DISTRICT,
    labels = DISTRICT_LABELS_EN,
    name   = "Districts"
  ) +
  scale_linetype_manual(
    values = c("Temperature (°C)" = "solid", "Relative Humidity (%)" = "dashed"),
    name = "Parameter"
  ) +
  scale_x_discrete(labels = translate_month_en) +
  scale_y_continuous(
    name = "Mean Temperature (°C)",
    sec.axis = sec_axis(~ . * 2.5, name = "Relative Humidity (%)")
  ) +
  labs(x     = NULL,
       title = paste("Seasonal Temperature and Humidity Profile —", SSP_LABEL_EN)) +
  theme_thesis() +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    legend.margin = margin(t = 0)
  )

ggsave("fig_temp_rh.png", p_temp_rh, path = DIR_FIG,
       width = 8.5, height = 5.5, dpi = 300)


## =========================================================
## 2) lambda_local Heatmap
## =========================================================
cat(">>> 2) lambda_local heatmap <<<\n")

heatmap_data <- monthly %>%
  group_by(district_label, month_name) %>%
  summarise(lam = mean(lambda_local_i1_mean, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(lam_plot = ifelse(lam < 1e-8, NA_real_, lam))

fig_heatmap <- ggplot(heatmap_data, aes(x = month_name, y = district_label,
                                        fill = lam_plot)) +
  geom_tile(colour = "white") +
  scale_fill_gradientn(
    colours  = c("grey92", "#FFF5B1", "#FEB24C", "#FC4E2A", "#B10026"),
    trans    = "log10",
    na.value = "grey88",
    breaks   = c(1e-5, 1e-4, 1e-3, 1e-2, 1e-1),
    labels   = label_scientific(),
    name     = expression(lambda[local]~"(log"[10]*" scale)"),
    guide    = guide_colorbar(
      barwidth       = 20,
      barheight      = 1.2,
      title.position = "top",
      title.hjust    = 0.5
    )
  ) +
  scale_x_discrete(labels = translate_month_en) +
  scale_y_discrete(labels = translate_district_en) +
  labs(
    x       = NULL,
    y       = NULL,
    title   = paste("Monthly local (autochthonous) transmission rate —", SSP_LABEL_EN),
    caption = paste0(
      "■ Dark grey: Below thermal threshold (λ < 10⁻⁸) — autochthonous transmission impossible\n",
      "■ Light yellow: Marginal early/late season (10⁻⁵–10⁻³)\n",
      "■ Orange-red: Active transmission season (λ ≥ 10⁻²)"
    )
  ) +
  theme_thesis() +
  theme(
    axis.text.x  = element_text(angle = 45, hjust = 1),
    plot.caption = element_text(
      size    = 8.5,
      colour  = "grey30",
      hjust   = 0,
      margin  = margin(t = 10)
    ),
    legend.title = element_text(size = 9, hjust = 0.5)
  )

ggsave("fig_heatmap_lambda.png", fig_heatmap, path = DIR_FIG,
       width = 9, height = 5.5, dpi = 300)


## =========================================================
## 3) P_est Seasonality
## ---------------------------------------------------------
## NOTE: the p_establishment_mean > 0 filter is applied BEFORE
## aggregation, so the monthly mean is taken only over years in which
## that month is active. This is a deliberate choice (so thermally
## dead months don't artificially pull the seasonal profile to zero),
## but the number of years underlying the mean at the start/end of the
## season is small. This should be stated in the figure caption.
## =========================================================
cat(">>> 3) P_est seasonality <<<\n")

pest_season <- monthly %>%
  filter(p_establishment_mean > 0) %>%
  group_by(district_id, month_name) %>%
  summarise(pest_mean = mean(p_establishment_mean, na.rm = TRUE),
            n_years   = dplyr::n(),
            .groups = "drop") %>%
  censor_log("pest_mean", label = "P_est seasonal")

fig_pest_season <- ggplot(filter(pest_season, !is.na(pest_mean)),
                          aes(x = month_name, y = pest_mean,
                              group = district_id,
                              colour = district_id)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.5) +
  scale_y_log10(labels = label_scientific()) +
  scale_x_discrete(labels = translate_month_en) +
  scale_colour_manual(values = COL_DISTRICT, labels = DISTRICT_LABELS_EN, name = NULL) +
  labs(x = NULL, y = expression(P[est]~"(log scale)"),
       title = paste("Establishment probability seasonal profile —", SSP_LABEL_EN),
       caption = "The mean is taken only over years in which the corresponding month is active.") +
  theme_thesis() +
  theme(plot.caption = element_text(size = 8, colour = "grey40", hjust = 0))

ggsave("fig_pest_season.png", fig_pest_season, path = DIR_FIG,
       width = 8, height = 5, dpi = 300)


## =========================================================
## 4) Annual Risk Trend
## ---------------------------------------------------------
## v2 FIXES:
##   (a) scale_y_log10 SILENTLY drops zero/negative rows. In SSP5-8.5 /
##       Hopa / 2028, p_ge1_major_year_mean == 0. Now explicitly set to
##       NA and reported via censor_log().
##   (b) Values below ~1.1e-16 are machine-epsilon artefacts; cut with
##       NOISE_FLOOR and the axis is pinned to this floor.
##   (c) The shaded ribbon is a MONTE CARLO SAMPLING INTERVAL, not a
##       parameter confidence interval. Stated in the caption.
##
## This section folds in what the Turkish original keeps as a separate
## "4-EN" English twin (fig_yearly_risk_en.png) alongside its Turkish
## fig_yearly_risk.png -- since this whole script is English-only, the
## two are merged into one, saved here under the shared base filename
## fig_yearly_risk.png (inside figures_en/, so nothing collides with
## the original outputs/{ssp}/figures/ tree).
## =========================================================
cat(">>> 4) Annual risk trend <<<\n")

yearly_plot <- yearly %>%
  censor_log("p_ge1_major_year_mean", label = "annual risk (mean)") %>%
  censor_log("p_ge1_major_year_p2_5", label = "annual risk (p2.5)") %>%
  censor_log("p_ge1_major_year_p97_5", label = "annual risk (p97.5)") %>%
  filter(!is.na(p_ge1_major_year_mean))

n_dropped <- nrow(yearly) - nrow(yearly_plot)

y_lo <- max(NOISE_FLOOR, min(yearly_plot$p_ge1_major_year_mean, na.rm = TRUE))
y_hi <- max(yearly_plot$p_ge1_major_year_mean, na.rm = TRUE)

cap_en <- paste0(
  "Shaded band = 2.5th-97.5th percentile of outer Monte Carlo replicates ",
  "(sampling interval; NOT a parameter confidence interval).\n",
  "Numerical noise floor is 10⁻¹⁵; cells below this value are not plotted",
  if (n_dropped > 0) paste0(" (", n_dropped, " district-years).") else "."
)

fig_yearly <- ggplot(yearly_plot,
                     aes(x = year, y = p_ge1_major_year_mean,
                         colour = district_id, fill = district_id)) +
  geom_ribbon(aes(ymin = p_ge1_major_year_p2_5,
                  ymax = p_ge1_major_year_p97_5),
              alpha = 0.15, colour = NA, na.rm = TRUE) +
  geom_line(linewidth = 0.8, na.rm = TRUE) +
  geom_hline(yintercept = NOISE_FLOOR, linetype = "dotted",
             colour = "grey55", linewidth = 0.4) +
  annotate("text", x = min(yearly_plot$year), y = NOISE_FLOOR,
           label = "numerical noise floor", hjust = 0, vjust = -0.6,
           size = 2.7, colour = "grey45") +
  scale_y_log10(labels = label_scientific(),
                limits = c(min(y_lo, NOISE_FLOOR), y_hi),
                breaks = 10^seq(-16, 0, 2)) +
  scale_colour_manual(values = COL_DISTRICT, labels = DISTRICT_LABELS_EN, name = NULL) +
  scale_fill_manual(values   = COL_DISTRICT, labels = DISTRICT_LABELS_EN, guide = "none") +
  labs(x = "Year",
       y = expression(P["≥1 major/year"] ~ "(log"[10]*" scale)"),
       title = paste("Annual outbreak risk —", SSP_LABEL_EN),
       caption = cap_en) +
  theme_thesis() +
  theme(plot.caption = element_text(size = 8, colour = "grey40", hjust = 0,
                                    margin = margin(t = 8)))

ggsave("fig_yearly_risk.png", fig_yearly, path = DIR_FIG,
       width = 8, height = 5, dpi = 300)

# Record of censored rows (in case it comes up in defense/review)
if (n_dropped > 0) {
  yearly %>%
    filter(p_ge1_major_year_mean < NOISE_FLOOR | is.na(p_ge1_major_year_mean)) %>%
    select(district_id, year, p_ge1_major_year_mean,
           Lambda_import_year, mean_p_est_year_mean) %>%
    write_csv(file.path(DIR_TBL, "tbl_yearly_censored_rows.csv"))
}


## =========================================================
## 5) Horizon Table
## =========================================================
cat(">>> 5) Horizon risk table <<<\n")

horizon_tbl <- horizon %>%
  select(district_label,
         p_mean = p_ge1_major_mean,
         p_lo = p_ge1_major_p2_5,
         p_hi = p_ge1_major_p97_5,
         Lambda = Lambda_import) %>%
  arrange(desc(p_mean)) %>%
  mutate(across(c(p_mean, p_lo, p_hi), ~formatC(.x, format = "e", digits = 2)),
         Lambda = round(Lambda, 1)) %>%
  mutate(district_label = translate_district_en(district_label)) %>%
  rename(`District` = district_label,
         `P_horizon (MC mean)` = p_mean,
         `MC 2.5%` = p_lo,
         `MC 97.5%` = p_hi,
         `Lambda_import (51 years)` = Lambda)

write_csv(horizon_tbl, file.path(DIR_TBL, "tbl_horizon.csv"))

ft <- flextable(horizon_tbl) %>%
  autofit() %>%
  theme_vanilla() %>%
  flextable::add_footer_lines(
    "MC 2.5% / 97.5%: outer Monte Carlo sampling interval; NOT a parameter confidence interval.")
doc <- read_docx() %>% body_add_flextable(ft)
print(doc, target = file.path(DIR_TBL, "tbl_horizon.docx"))


## =========================================================
## 5b) Importation Pressure Summary (lambda_import trend + cumulative Lambda)
## =========================================================
cat(">>> 5b) Importation pressure summary <<<\n")

# ---- 5b.1) Annual importation pressure trend (log scale) ----
import_plot <- censor_log(yearly, "Lambda_import_year", label = "annual Lambda") %>%
  filter(!is.na(Lambda_import_year))

fig_import_trend <- ggplot(import_plot,
                           aes(x = year, y = Lambda_import_year,
                               colour = district_id)) +
  geom_line(linewidth = 0.7) +
  geom_smooth(method = "loess", formula = y ~ x, se = FALSE, linewidth = 0.5,
              linetype = "dashed", alpha = 0.5) +
  scale_y_log10(labels = label_scientific()) +
  scale_colour_manual(values = COL_DISTRICT,
                      labels = DISTRICT_LABELS_EN,
                      name = NULL) +
  labs(x = "Year",
       y = expression(Lambda["import, annual"]~"(log scale)"),
       title = paste("Annual importation pressure —", SSP_LABEL_EN)) +
  theme_thesis()

ggsave("fig_import_trend.png", fig_import_trend, path = DIR_FIG,
       width = 8, height = 5, dpi = 300)

# ---- 5b.2) Monthly importation pressure heatmap ----
import_heat <- monthly %>%
  group_by(district_label, month_name) %>%
  summarise(lambda_mean = mean(lambda_import, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(lambda_mean = ifelse(lambda_mean <= 0, NA_real_, lambda_mean))

fig_import_heat <- ggplot(import_heat,
                          aes(x = month_name, y = district_label,
                              fill = lambda_mean)) +
  geom_tile(colour = "white", linewidth = 0.3) +
  scale_fill_viridis_c(option = "inferno", trans = "log10",
                       labels = label_scientific(),
                       na.value = "grey88",
                       name = expression(lambda["import"])) +
  scale_x_discrete(labels = translate_month_en) +
  scale_y_discrete(labels = translate_district_en) +
  labs(x = NULL, y = NULL,
       title = paste("Mean monthly importation pressure —", SSP_LABEL_EN)) +
  theme_thesis() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("fig_import_heatmap.png", fig_import_heat, path = DIR_FIG,
       width = 8, height = 4, dpi = 300)

# ---- 5b.3) Cumulative Lambda_import summary table ----
import_summary <- horizon %>%
  select(district_label, Lambda_import) %>%
  arrange(desc(Lambda_import)) %>%
  mutate(
    Lambda_fmt = round(Lambda_import, 1),
    Comment = case_when(
      Lambda_import >= 100 ~ "High importation pressure",
      Lambda_import >= 10  ~ "Moderate importation pressure",
      Lambda_import >= 1   ~ "Low importation pressure",
      TRUE                 ~ "Very low (< 1 expected case/51 years)"
    ),
    district_label = translate_district_en(district_label)
  ) %>%
  select(`District` = district_label,
         `Lambda_import (51 years)` = Lambda_fmt,
         Comment)

write_csv(import_summary, file.path(DIR_TBL, "tbl_import_summary.csv"))

ft_imp <- flextable(import_summary) %>% autofit() %>% theme_vanilla()
doc_imp <- read_docx() %>% body_add_flextable(ft_imp)
print(doc_imp, target = file.path(DIR_TBL, "tbl_import_summary.docx"))

cat("  Importation pressure figures and table saved.\n")


## =========================================================
## 6) STAGE 2 -- Predictor Comparison (sigma_EIP > 0)
## ---------------------------------------------------------
## v2 NOTE: v1 labelled this section "Model validation". But the two
## quantities compared here are:
##   - p_establishment_mean = E[P_est(EIP)]   (stochastic EIP, primary)
##   - P_est_analytic       = P_est(E[EIP])   (substitution)
## This is STAGE 2 in the thesis (predictor comparison). STAGE 1
## (software verification) requires a separate sigma_EIP = 0 run and is
## NOT performed in this script; see validation_two_stage.R.
## =========================================================
cat(">>> 6) Stage 2 -- predictor comparison <<<\n")

val_df <- monthly %>%
  mutate(
    rho_ratio = ifelse(lambda_local_i1_mean > 0,
                       gamma_val / lambda_local_i1_mean, Inf),
    P_est_analytic = case_when(
      lambda_local_i1_mean <= 0  ~ 0,
      abs(rho_ratio - 1) < 1e-10 ~ 1 / TAU_VAL,
      TRUE ~ (1 - rho_ratio) / (1 - rho_ratio^TAU_VAL)
    ),
    abs_diff = abs(p_establishment_mean - P_est_analytic),
    signed_diff = P_est_analytic - p_establishment_mean,
    active   = lambda_local_i1_mean > 0
  )

val_active <- filter(val_df, active)

# Jensen direction: the thesis claims P_est(E[EIP]) > E[P_est(EIP)] in
# ALL active cells. Verified here.
n_direction_violations <- sum(val_active$signed_diff < 0, na.rm = TRUE)

val_summary <- tibble(
  Metric = c("Total combinations", "Active (lambda>0)",
             "Mean |difference|", "Max |difference|", "< 0.02 threshold",
             "Jensen direction violation (P_est(E[EIP]) < E[P_est(EIP)])"),
  Value  = c(nrow(val_df), nrow(val_active),
            formatC(mean(val_active$abs_diff), format = "e", digits = 2),
            formatC(max(val_active$abs_diff), format = "e", digits = 2),
            paste0(sum(val_active$abs_diff < 0.02), "/", nrow(val_active)),
            paste0(n_direction_violations, "/", nrow(val_active)))
)

write_csv(val_summary, file.path(DIR_TBL, "tbl_validation.csv"))

if (n_direction_violations > 0)
  warning(sprintf("[STAGE 2] The Jensen direction reversed in %d active cells. The thesis\n",
                  n_direction_violations),
          "  text states the direction 'never reverses in any cell'; it should be updated.",
          call. = FALSE)

val_plot <- val_active %>%
  censor_log("P_est_analytic",       label = "P_est analytic") %>%
  censor_log("p_establishment_mean", label = "P_est MC") %>%
  filter(!is.na(P_est_analytic), !is.na(p_establishment_mean))

fig_val <- ggplot(val_plot, aes(x = P_est_analytic,
                                y = p_establishment_mean,
                                colour = district_label)) +
  geom_point(alpha = 0.4, size = 1) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "red") +
  scale_x_log10(labels = label_scientific()) +
  scale_y_log10(labels = label_scientific()) +
  scale_colour_manual(values = COL_DISTRICT_LABEL, labels = translate_district_en, name = NULL) +
  labs(x = expression(P[est](E*"["*EIP*"]")~" (substitution)"),
       y = expression(E*"["*P[est](EIP)*"]"~" (primary, MC)"),
       title = paste("Stage 2 — predictor comparison —", SSP_LABEL_EN),
       caption = paste0("Cells below the 10⁻¹⁵ noise floor are not plotted. ",
                        "This is not a software verification (Stage 1).")) +
  theme_thesis() +
  theme(plot.caption = element_text(size = 8, colour = "grey40", hjust = 0))

ggsave("fig_validation.png", fig_val, path = DIR_FIG,
       width = 7, height = 5, dpi = 300)


## =========================================================
## 7) Importation Sensitivity (k + eta)
## =========================================================
cat(">>> 7) Importation sensitivity <<<\n")

sens_imp <- load_sens_imp(SSP)
if (!is.null(sens_imp)) {
  write_csv(sens_imp, file.path(DIR_TBL, "tbl_sens_importation.csv"))
  cat("  Importation sensitivity table saved.\n")
} else {
  cat("  No importation sensitivity data found -- skipping.\n")
}


## =========================================================
## 8) Model Sensitivity (m, beta_vh, IP)
## =========================================================
cat(">>> 8) Model sensitivity <<<\n")

sens_mc <- load_sens_mc(SSP)
tornado_path <- file.path(here("outputs", SSP, "sensitivity", "ctmc_mc"),
                          "sensitivity_tornado.csv")

if (!is.null(sens_mc) && file.exists(tornado_path)) {
  sens_tor <- read_csv(tornado_path, show_col_types = FALSE) %>%
    mutate(
      signed_log10  = sign(mean_delta_pct) * log10(abs(mean_delta_pct) + 1),
      scenario_label_tr = dplyr::recode(scenario, !!!SCEN_TR),
      scenario_label_en = translate_scenario_label_en(scenario_label_tr)
    )

  max_abs <- max(abs(sens_tor$signed_log10), na.rm = TRUE) * 1.1

  fig_tornado <- ggplot(sens_tor,
                        aes(x = reorder(scenario_label_en, abs(signed_log10)),
                            y = signed_log10)) +
    geom_col(aes(fill = signed_log10 > 0), show.legend = FALSE, width = 0.7) +
    geom_text(aes(label = sprintf("%+.0f%%", mean_delta_pct),
                  hjust = ifelse(signed_log10 >= 0, -0.1, 1.1)),
              size = 3, colour = "grey30") +
    coord_flip(ylim = c(-max_abs, max_abs)) +
    scale_fill_manual(values = c("TRUE" = "#E63946", "FALSE" = "#457B9D")) +
    labs(x = NULL,
         y = expression("Change from baseline scenario — sign(x) " %*% " log"[10]*"(|%Δ|+1)"),
         title = paste("Parameter sensitivity —", SSP_LABEL_EN),
         caption = "Labels next to each bar show the original % change.") +
    theme_thesis() +
    theme(axis.text.y = element_text(size = 9))

  ggsave("fig_tornado.png", fig_tornado, path = DIR_FIG,
         width = 8, height = 5, dpi = 300)
  cat("  Tornado plot (log10) saved.\n")
} else {
  cat("  No model sensitivity data found -- skipping.\n")
}


## =========================================================
## 9) GBD Country Contributions
## =========================================================
cat(">>> 9) GBD country contributions <<<\n")

cc <- load_country_contrib(SSP)
if (!is.null(cc)) {
  top_25 <- cc %>% slice_max(contribution_pct, n = 25)

  fig_country <- ggplot(top_25,
                        aes(x = reorder(country, contribution_pct),
                            y = contribution_pct)) +
    geom_col(fill = "#E63946", alpha = 0.85) +
    coord_flip() +
    labs(x = NULL, y = "Contribution to importation risk (%)",
         title = "Dengue importation risk by source country",
         subtitle = paste("GBD 2023 × tourist weighting —", SSP_LABEL_EN)) +
    theme_thesis()

  ggsave("fig_country_contrib.png", fig_country, path = DIR_FIG,
         width = 7, height = 5, dpi = 300)

  write_csv(top_25, file.path(DIR_TBL, "tbl_country_contrib.csv"))

  # Canonical value for the "Brazil ~40% / 42.6%" inconsistency in the thesis text
  br <- cc %>% filter(grepl("^Brazil|Brezilya", country, ignore.case = TRUE))
  if (nrow(br) == 1)
    cat(sprintf("  >> Brazil's contribution share = %%%.1f  (the thesis text should be fixed to this ONE value)\n",
                br$contribution_pct[1]))

  cat("  Country contribution figure and table saved.\n")
} else {
  cat("  No country contribution data found -- skipping.\n")
}


## =========================================================
## 10) LHS-PRCC Sensitivity Analysis (SSP-specific climate data)
## ---------------------------------------------------------
## !!! WARNING -- CONTRADICTS THE THESIS TEXT !!!
##
## The PRCC OUTPUT VARIABLE in this section is R0, NOT P_horizon.
## compute_R0_lhs() produces an R0 value and PRCC is taken against that
## R0. Computing P_horizon would require importation pressure, 612
## months of accumulation, and finite-threshold dynamics -- none of
## which happen in this section.
##
## The following thesis-text statements DO NOT MATCH this code:
##   - Section 6.9: "...variability in the model output P_horizon..."
##   - Figure 6.9 caption: "...the partial rank correlation coefficient
##     of each input with P_horizon..."
##   - Appendix A.5 (current version): "PRCC, based on the P_horizon
##     output..."
##
## The correct statement is R0. Either (a) these three thesis passages
## should be corrected to R0, or (b) the analysis should genuinely be
## rebuilt on P_horizon. If (a) is chosen, Appendix A.5's ORIGINAL
## wording ("based on the R0 output") was correct.
##
## Also note: this section uses Ae. albopictus thermal parameters for
## ALL cells; it is not species-specific (Hopa and Zonguldak are
## modelled with Ae. aegypti). A caveat analogous to the Appendix A.3
## CSI warning is needed here too.
##
## v2 FIX: the confidence interval is now a genuine bootstrap. v1 used
## cor.test()$conf.int (Fisher z transform); the thesis text says
## "95% bootstrap confidence intervals". Point estimates are UNCHANGED.
## =========================================================
cat(">>> 10) LHS-PRCC sensitivity analysis (output = R0) <<<\n")
cat("  [WARNING] The PRCC output is R0. Any 'P_horizon' wording in the thesis\n")
cat("            text (Section 6.9, Figure 6.9 caption, Appendix A.5) needs correction.\n")

library(lhs)

set.seed(123)
n_lhs   <- 2000
n_boot  <- 2000

# ---- SSP-specific: sample T and RH from the real climate data ----
active_climate <- monthly %>%
  filter(lambda_local_i1_mean > 0) %>%
  select(temp_c, rh) %>%
  filter(is.finite(temp_c), is.finite(rh))

if (nrow(active_climate) < 100) {
  active_climate <- monthly %>%
    select(temp_c, rh) %>%
    filter(is.finite(temp_c), is.finite(rh))
}

climate_sample_idx <- sample(nrow(active_climate), n_lhs, replace = TRUE)
T_sampled  <- active_climate$temp_c[climate_sample_idx]
RH_sampled <- active_climate$rh[climate_sample_idx]

T_range  <- round(range(T_sampled), 1)
RH_range <- round(range(RH_sampled), 1)

cat("  SSP-specific climate (bootstrap sampling):\n")
cat("    T_C  :", T_range[1], "-", T_range[2], "°C\n")
cat("    RH   :", RH_range[1], "-", RH_range[2], "%\n")
cat("    Active-month pool:", nrow(active_climate), "observations\n")

# ---- Remaining parameters: uniform LHS sampling ----
param_ranges_other <- list(
  m       = c(0.10, 2.00),
  beta_vh = c(0.10, 0.60),
  beta_hv = c(0.10, 0.60),
  ip_days = c(3, 10)
)

lhs_unit <- randomLHS(n_lhs, length(param_ranges_other))
colnames(lhs_unit) <- names(param_ranges_other)

lhs_params <- as.data.frame(lhs_unit)
for (nm in names(param_ranges_other)) {
  rng <- param_ranges_other[[nm]]
  lhs_params[[nm]] <- rng[1] + lhs_unit[, nm] * (rng[2] - rng[1])
}

lhs_params$T_C <- T_sampled
lhs_params$RH  <- RH_sampled

# Mordecai 2017 Ae. albopictus parameters (used for ALL cells; see caveat above)
C_a   <- 1.93e-4; T0_a  <- 10.25; Tm_a  <- 38.32
C_eip <- 1.09e-4; T0_e  <- 10.39; Tm_e  <- 43.05
C_lf  <- 1.43e-1; T0_lf <- 6.24;  Tm_lf <- 38.25
k_vpd <- 0.5; SVPD_ref <- 1.0; lf_floor <- 0.25

briere_fn  <- function(T, c, T0, Tm) ifelse(T > T0 & T < Tm, c * T * (T - T0) * sqrt(Tm - T), 0)
quad_lf_fn <- function(T, c, T0, Tm) pmax(c * (T - T0) * (Tm - T), lf_floor)
svpd_fn    <- function(T, RH) pmax(0.6108 * exp(17.27 * T / (T + 237.3)) * (1 - RH / 100), 0)

# v2: vectorised (v1 returned rows one at a time via apply(); slow and
# fragile if a new column was ever added to lhs_params)
compute_R0_vec <- function(T_C, RH, m, beta_vh, beta_hv, ip_days) {
  a    <- briere_fn(T_C, C_a, T0_a, Tm_a)
  lf   <- quad_lf_fn(T_C, C_lf, T0_lf, Tm_lf)
  eip  <- 1 / pmax(briere_fn(T_C, C_eip, T0_e, Tm_e), 1e-6)
  vpd  <- svpd_fn(T_C, RH)
  mu_v <- (1 / lf) * exp(k_vpd * (vpd - SVPD_ref))
  gam  <- 1 / ip_days
  pmax(m * a^2 * beta_vh * beta_hv * exp(-mu_v * eip) / (mu_v * gam), 0)
}

lhs_params$R0 <- with(lhs_params,
                      compute_R0_vec(T_C, RH, m, beta_vh, beta_hv, ip_days))

param_names <- c("m", "beta_vh", "beta_hv", "T_C", "RH", "ip_days")

# Core function computing a single PRCC value (also used inside the bootstrap)
prcc_one <- function(dat, nm, params = param_names) {
  others      <- setdiff(params, nm)
  rank_xi     <- rank(dat[[nm]])
  rank_y      <- rank(dat$R0)
  rank_others <- as.data.frame(lapply(dat[, others, drop = FALSE], rank))
  resid_xi <- residuals(lm(rank_xi ~ ., data = rank_others))
  resid_y  <- residuals(lm(rank_y  ~ ., data = rank_others))
  unname(cor(resid_xi, resid_y))
}

prcc_result <- purrr::map_dfr(param_names, function(nm) {
  est <- prcc_one(lhs_params, nm)

  # --- v2: genuine bootstrap confidence interval ---
  boot_vals <- vapply(seq_len(n_boot), function(b) {
    idx <- sample.int(nrow(lhs_params), replace = TRUE)
    prcc_one(lhs_params[idx, , drop = FALSE], nm)
  }, numeric(1))

  ci <- stats::quantile(boot_vals, c(0.025, 0.975), na.rm = TRUE)

  # p-value: parametric (Fisher z), kept for reference
  ct <- suppressWarnings(cor.test(
    residuals(lm(rank(lhs_params[[nm]]) ~ .,
                 data = as.data.frame(lapply(
                   lhs_params[, setdiff(param_names, nm), drop = FALSE], rank)))),
    residuals(lm(rank(lhs_params$R0) ~ .,
                 data = as.data.frame(lapply(
                   lhs_params[, setdiff(param_names, nm), drop = FALSE], rank)))),
    method = "pearson"))

  tibble(parameter = nm,
         PRCC      = est,
         ci_lo     = unname(ci[1]),
         ci_hi     = unname(ci[2]),
         ci_type   = sprintf("bootstrap (B=%d)", n_boot),
         p_value   = format.pval(ct$p.value, digits = 3, eps = 1e-4),
         outcome   = "R0")
}) %>%
  arrange(desc(abs(PRCC)))

write_csv(prcc_result, file.path(DIR_TBL, "tbl_prcc.csv"))
cat("  PRCC ranking (output = R0):\n")
print(as.data.frame(prcc_result %>%
        mutate(across(c(PRCC, ci_lo, ci_hi), ~round(.x, 3)))), row.names = FALSE)

param_ranges_tbl <- tibble(
  Parameter = c("m", "beta_vh", "beta_hv", "ip_days", "T_C", "RH"),
  Lower = c(param_ranges_other$m[1],
            param_ranges_other$beta_vh[1],
            param_ranges_other$beta_hv[1],
            param_ranges_other$ip_days[1],
            T_range[1],
            RH_range[1]),
  Upper = c(param_ranges_other$m[2],
            param_ranges_other$beta_vh[2],
            param_ranges_other$beta_hv[2],
            param_ranges_other$ip_days[2],
            T_range[2],
            RH_range[2]),
  Sampling = c("LHS (uniform)", "LHS (uniform)", "LHS (uniform)", "LHS (uniform)",
               "Bootstrap (active-month pool)", "Bootstrap (active-month pool)"),
  Source = c("Literature", "Literature", "Literature", "Literature",
             paste0("Bootstrap (", SSP_LABEL_EN, " active months)"),
             paste0("Bootstrap (", SSP_LABEL_EN, " active months)"))
)

write_csv(param_ranges_tbl, file.path(DIR_TBL, "tbl_param_ranges.csv"))

fig_prcc <- ggplot(prcc_result, aes(x = reorder(parameter, abs(PRCC)), y = PRCC)) +
  geom_col(aes(fill = PRCC > 0), show.legend = FALSE, width = 0.7) +
  geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi), width = 0.2) +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = "#E63946", "FALSE" = "#457B9D")) +
  scale_x_discrete(labels = function(b) unname(PARAM_LABELS_EN[b])) +
  labs(x = NULL, y = expression("PRCC ("*R[0]*" output)"),
       title = paste(SSP_LABEL_EN, "LHS–PRCC sensitivity analysis"),
       subtitle = sprintf("n = %d | T: %.1f–%.1f°C | RH: %.1f–%.1f%% (active-month bootstrap)",
                          n_lhs, T_range[1], T_range[2],
                          RH_range[1], RH_range[2]),
       caption = paste0("Output variable is R₀ (not P_horizon). ",
                        "Error bars are 95% bootstrap confidence intervals (B = ", n_boot, ").\n",
                        "Thermal parameters are Ae. albopictus's for all samples.")) +
  theme_thesis() +
  theme(plot.caption = element_text(size = 8, colour = "grey40", hjust = 0))

ggsave("fig_prcc.png", fig_prcc, path = DIR_FIG, width = 7, height = 4.8, dpi = 300)


## =========================================================
## 11) CSI Heatmap and Trend
## ---------------------------------------------------------
## NOTE: as a species-neutral comparison metric, CSI is computed using
## only the Ae. albopictus curves. For Hopa and Zonguldak, which are
## assigned Ae. aegypti in the model, there is NO one-to-one species
## match between CSI and P_est. (This caveat is present in thesis
## Appendix A.3.)
## =========================================================
cat(">>> 11) CSI heatmap and trend <<<\n")

T_grid <- seq(0, 45, by = 0.1)
a_max_theory   <- max(briere_fn(T_grid, C_a, T0_a, Tm_a))
eip_max_theory <- max(briere_fn(T_grid, C_eip, T0_e, Tm_e))
lf_max_theory  <- max(quad_lf_fn(T_grid, C_lf, T0_lf, Tm_lf))

monthly_csi <- monthly %>%
  mutate(
    a_norm   = pmax(briere_fn(temp_c, C_a, T0_a, Tm_a), 0) / a_max_theory,
    lf_norm  = pmax(quad_lf_fn(temp_c, C_lf, T0_lf, Tm_lf), lf_floor) / lf_max_theory,
    eip_norm = pmax(briere_fn(temp_c, C_eip, T0_e, Tm_e), 0) / eip_max_theory,
    CSI = (a_norm + lf_norm + eip_norm) / 3
  )

csi_heat <- monthly_csi %>%
  group_by(district_label, month_name) %>%
  summarise(CSI_mean = mean(CSI, na.rm = TRUE), .groups = "drop")

fig_csi_heat <- ggplot(csi_heat, aes(x = month_name,
                                     y = fct_rev(factor(district_label)),
                                     fill = CSI_mean)) +
  geom_tile(colour = "white", linewidth = 0.6) +
  geom_text(aes(label = sprintf("%.2f", CSI_mean)), size = 2.8, colour = "grey20") +
  scale_fill_gradientn(
    colours = c("#EFF3FF","#BDD7E7","#6BAED6","#2171B5","#08306B"),
    name    = "Climate Suitability\nIndex (CSI)",
    limits  = c(0, 1),
    breaks  = c(0, 0.25, 0.50, 0.75, 1.00),
    labels  = c("0.00", "0.25", "0.50", "0.75", "1.00"),
    guide   = guide_colorbar(barwidth = 18, barheight = 1.0,
                             title.position = "top", title.hjust = 0.5,
                             label.hjust = 0.5)
  ) +
  scale_x_discrete(labels = translate_month_en) +
  scale_y_discrete(labels = translate_district_en) +
  labs(
    x       = "Month",
    y       = "District",
    title   = paste("Climate Suitability Index —", SSP_LABEL_EN),
    caption = paste0("CSI = (a_norm + lf_norm + eip_norm) / 3; Brière thermal ",
                     "performance curves, Mordecai 2017.\n",
                     "CSI is computed using the Ae. albopictus curves for all districts; ",
                     "Hopa and Zonguldak are modelled with Ae. aegypti.")
  ) +
  theme_thesis() +
  theme(
    panel.grid   = element_blank(),
    axis.ticks   = element_blank(),
    plot.caption = element_text(size = 8, colour = "grey40", hjust = 0,
                                margin = margin(t = 6))
  )

ggsave("fig_csi_heat.png", fig_csi_heat, path = DIR_FIG,
       width = 9, height = 5.2, dpi = 300)

csi_yearly <- monthly_csi %>%
  group_by(district_id, district_label, year) %>%
  summarise(CSI_year = mean(CSI, na.rm = TRUE), .groups = "drop")

fig_csi_trend <- ggplot(csi_yearly, aes(x = year, y = CSI_year,
                                        colour = district_id,
                                        fill   = district_id)) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, alpha = 0.15, linewidth = 1) +
  scale_colour_manual(values = COL_DISTRICT, labels = DISTRICT_LABELS_EN, name = NULL) +
  scale_fill_manual(values = COL_DISTRICT, labels = DISTRICT_LABELS_EN, guide = "none") +
  labs(x = "Year", y = "Mean CSI",
       title = paste("Annual CSI trend —", SSP_LABEL_EN)) +
  theme_thesis()

ggsave("fig_csi_trend.png", fig_csi_trend, path = DIR_FIG,
       width = 8, height = 5, dpi = 300)

write_csv(csi_heat, file.path(DIR_TBL, "tbl_csi_monthly.csv"))

# Thesis Discussion-section claim "Fethiye 0.83 > Kartal 0.79 at peak month"
csi_peak <- csi_heat %>%
  group_by(district_label) %>%
  slice_max(CSI_mean, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(desc(CSI_mean))
write_csv(csi_peak, file.path(DIR_TBL, "tbl_csi_peak.csv"))
cat("  Peak-month CSI values (compare against the thesis Discussion section):\n")
print(as.data.frame(csi_peak %>% mutate(CSI_mean = round(CSI_mean, 3))),
      row.names = FALSE)


## =========================================================
## 12) Moran's I Spatial Autocorrelation  (exploratory)
## ---------------------------------------------------------
## With n = 5, the statistical power of Moran's I is extremely low; if
## the result is not reported in the thesis it should remain
## exploratory.
## =========================================================
cat(">>> 12) Moran's I (exploratory, n=5) <<<\n")

if (requireNamespace("spdep", quietly = TRUE) &&
    requireNamespace("sf", quietly = TRUE) &&
    requireNamespace("ggrepel", quietly = TRUE)) {

  library(spdep); library(sf)

  # Canonical district-coordinate mapping (identical to 00_results_setup.R)
  district_coords <- data.frame(
    district_id = c("TUR.10.4_1","TUR.39.3_1","TUR.40.25_1","TUR.59.4_1","TUR.81.6_1"),
    label = c("Hopa","Eğirdir","Kartal","Fethiye","Zonguldak"),
    lon = c(41.12, 30.85, 29.19, 29.12, 31.79),
    lat = c(41.41, 37.88, 40.89, 36.62, 41.45)
  )

  # Verify this mapping is consistent with the setup file (protects
  # against a label swap). NOTE: this check intentionally compares
  # against the ORIGINAL (Turkish) DISTRICT_LABELS -- it is a data
  # integrity check, not display text, and must not be translated.
  stopifnot(identical(
    unname(DISTRICT_LABELS[district_coords$district_id]),
    c("Hopa (Artvin)", "Eğirdir (Isparta)", "Kartal (İstanbul)",
      "Fethiye (Muğla)", "Zonguldak")
  ))

  pts_sf <- st_as_sf(district_coords, coords = c("lon","lat"), crs = 4326) %>%
    st_transform(crs = 32636)

  coords_mat <- st_coordinates(pts_sf)
  knn2 <- knearneigh(coords_mat, k = 2)
  nb2  <- knn2nb(knn2)
  w2   <- nb2listw(nb2, style = "W")

  horizon_vec <- horizon %>%
    arrange(factor(as.character(district_id), levels = district_coords$district_id)) %>%
    pull(p_ge1_major_mean)

  csi_vec <- monthly_csi %>%
    group_by(district_id) %>%
    summarise(CSI_mean = mean(CSI, na.rm = TRUE), .groups = "drop") %>%
    arrange(factor(as.character(district_id), levels = district_coords$district_id)) %>%
    pull(CSI_mean)

  mt_p   <- moran.test(log10(pmax(horizon_vec, NOISE_FLOOR)), w2,
                       randomisation = TRUE, alternative = "two.sided")
  mt_csi <- moran.test(csi_vec, w2, randomisation = TRUE, alternative = "two.sided")

  moran_tbl <- tibble(
    Variable    = c("log10(p_ge1_major)", "CSI"),
    `Moran's I` = c(round(mt_p$estimate[1], 4), round(mt_csi$estimate[1], 4)),
    `p-value`   = c(format.pval(mt_p$p.value, digits = 3),
                     format.pval(mt_csi$p.value, digits = 3)),
    Comment     = c(ifelse(mt_p$p.value < 0.05, "Significant clustering",
                     "No significant autocorrelation (n=5, limited power)"),
                    ifelse(mt_csi$p.value < 0.05, "Significant clustering",
                     "No significant autocorrelation (n=5, limited power)"))
  )

  write_csv(moran_tbl, file.path(DIR_TBL, "tbl_moran.csv"))

  z_p     <- scale(log10(pmax(horizon_vec, NOISE_FLOOR)))[,1]
  z_csi   <- scale(csi_vec)[,1]
  lag_p   <- lag.listw(w2, z_p)
  lag_csi <- lag.listw(w2, z_csi)

  moran_df <- tibble(label = translate_district_en(district_coords$label),
                     z_p = z_p, lag_p = lag_p,
                     z_csi = z_csi, lag_csi = lag_csi)

  p_moran <- ggplot(moran_df, aes(x = z_p, y = lag_p)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey60") +
    geom_vline(xintercept = 0, linetype = "dashed", colour = "grey60") +
    geom_smooth(method = "lm", formula = y ~ x, se = TRUE,
                colour = "#E63946", alpha = 0.15) +
    geom_point(size = 4, colour = "#E63946", shape = 21, fill = "white", stroke = 1.5) +
    ggrepel::geom_text_repel(aes(label = label), size = 3.2) +
    labs(x = "Std. log10(p_ge1_major)", y = "Spatial lag",
         title = paste("Moran scatterplot —", SSP_LABEL_EN),
         caption = "Exploratory: n = 5, statistical power is limited.") +
    theme_thesis() +
    theme(plot.caption = element_text(size = 8, colour = "grey40", hjust = 0))

  ggsave("fig_moran.png", p_moran, path = DIR_FIG, width = 6, height = 5, dpi = 300)
  cat("  Moran's I complete.\n")
} else {
  cat("  spdep/sf/ggrepel packages not available -- skipping Moran's I.\n")
}


## =========================================================
## 13) Decadal Risk Progression
## =========================================================
cat(">>> 13) Decadal risk progression <<<\n")

decade_df <- yearly %>%
  mutate(
    decade_year = floor(year / 10) * 10,
    decade = factor(
      decade_year,
      levels = seq(2020, 2070, 10),
      labels = c("2020s","2030s","2040s",
                 "2050s","2060s","2070s")
    )
  ) %>%
  filter(!is.na(decade)) %>%
  group_by(district_id, district_label, decade) %>%
  summarise(p_mean = mean(p_ge1_major_year_mean, na.rm = TRUE), .groups = "drop") %>%
  censor_log("p_mean", label = "decadal mean risk")

dec_lo <- max(NOISE_FLOOR, min(decade_df$p_mean, na.rm = TRUE))

fig_decade <- ggplot(filter(decade_df, !is.na(p_mean)),
                     aes(x = decade, y = p_mean,
                         colour = district_id, group = district_id)) +
  geom_line(linewidth = 0.8, alpha = 0.7) +
  geom_point(size = 3) +
  scale_colour_manual(values = COL_DISTRICT, labels = DISTRICT_LABELS_EN,
                      name = "District") +
  scale_y_log10(
    labels = label_scientific(digits = 1),
    breaks = 10^seq(-16, 0, 2),
    limits = c(dec_lo, NA)
  ) +
  labs(
    title = paste("Decadal risk progression —", SSP_LABEL_EN),
    x     = "Decade",
    y     = "Mean annual risk (log₁₀)"
  ) +
  theme_thesis() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

ggsave("fig_decade.png", fig_decade, path = DIR_FIG,
       width = 9, height = 5, dpi = 300)

decade_wide <- decade_df %>%
  select(district_label, decade, p_mean) %>%
  mutate(district_label = translate_district_en(district_label)) %>%
  pivot_wider(names_from = decade, values_from = p_mean) %>%
  rename("District" = district_label)

write_csv(decade_wide, file.path(DIR_TBL, "tbl_decade.csv"))
cat("  tbl_decade.csv written:", unique(yearly$ssp), "\n")


## =========================================================
## 14) Change in Transmission Season Length
## ---------------------------------------------------------
## v2 FIX: v1's axis label read "lambda_local > 0", but the computation
## actually used LAMBDA_THRESH = 1e-4. The thesis definition is also
## 1e-4 ("transmission season = number of months with lambda_local >
## 10^-4"). The label was brought into agreement with the computation
## and the thesis.
## =========================================================
cat(">>> 14) Transmission season length <<<\n")

LAMBDA_THRESH <- 1e-4

season_yr <- monthly %>%
  group_by(district_id, district_label, year) %>%
  summarise(
    season_len = sum(lambda_local_i1_mean > LAMBDA_THRESH, na.rm = TRUE),
    peak_month = if (!is.null(AY_EN)) AY_EN[which.max(lambda_local_i1_mean)]
                 else AY_TR[which.max(lambda_local_i1_mean)],
    .groups = "drop"
  )

fig_season <- ggplot(season_yr, aes(x = year, y = season_len,
                                    colour = district_id, fill = district_id)) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, alpha = 0.15, linewidth = 1.2) +
  scale_colour_manual(values = COL_DISTRICT, labels = DISTRICT_LABELS_EN, name = "District") +
  scale_fill_manual(values = COL_DISTRICT, labels = DISTRICT_LABELS_EN, name = "District") +
  scale_y_continuous(breaks = 0:12) +
  labs(title = paste("Transmission season length —", SSP_LABEL_EN),
       x = "Year",
       y = expression("Number of active months ("*lambda[local]*" > 10"^-4*")")) +
  theme_thesis()

ggsave("fig_season.png", fig_season, path = DIR_FIG, width = 8, height = 5, dpi = 300)

season_comp <- season_yr %>%
  mutate(period = case_when(
    year >= 2025 & year <= 2035 ~ "Early",
    year >= 2065 & year <= 2075 ~ "Late",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(period)) %>%
  group_by(district_label, period) %>%
  summarise(season_mean = mean(season_len, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = period, values_from = season_mean) %>%
  mutate(
    Early = round(Early, 1),
    Late  = round(Late,  1),
    delta = round(Late - Early, 1),
    district_label = translate_district_en(district_label)
  ) %>%
  rename(District = district_label)

write_csv(season_comp, file.path(DIR_TBL, "tbl_season.csv"))

# Peak-month shift (a verifiable basis for the thesis Discussion-section
# claim "June-October -> May-November")
peak_shift <- season_yr %>%
  mutate(period = case_when(
    year >= 2025 & year <= 2035 ~ "Early",
    year >= 2065 & year <= 2075 ~ "Late",
    TRUE ~ NA_character_)) %>%
  filter(!is.na(period)) %>%
  count(district_label, period, peak_month) %>%
  group_by(district_label, period) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(district_label = translate_district_en(district_label)) %>%
  rename(District = district_label)
write_csv(peak_shift, file.path(DIR_TBL, "tbl_peak_month_shift.csv"))


## =========================================================
## 15) Multiple Regression -- on ANNUAL RISK (EXPLORATORY)
## ---------------------------------------------------------
## !!! THIS SECTION DOES NOT PRODUCE THE THESIS'S SECTION 6.10
##     REGRESSION TABLES !!!
##
## Here the dependent variable is log10(p_ge1_major_year_mean). The
## thesis's Section 6.10 is built on log10(P_est) instead, and its
## canonical source is 6_10_pest_regression.R. Outputs here are written
## with a "_yearlyrisk" suffix to avoid a filename clash.
##
## v2 FIX -- T_opt is now computed from M2:
## v1 derived T_opt from M3, which includes log10(Lambda_import + 1).
## Lambda_import is strongly collinear with temperature (via M_climate),
## and this produces the "OLS T_opt ~ 19 C" artefact the thesis treats
## as invalid. The thesis's canonical OLS T_opt value (25.6 C) comes
## from M2.
## =========================================================
cat(">>> 15) Multiple regression -- annual risk (EXPLORATORY) <<<\n")

stat_df <- monthly %>%
  filter(lambda_local_i1_mean > 0) %>%
  group_by(district_id, district_label, year) %>%
  summarise(
    T_season    = mean(temp_c, na.rm = TRUE),
    RH_season   = mean(rh, na.rm = TRUE),
    n_active    = dplyr::n(),
    pest_season = mean(p_establishment_mean, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(
    yearly %>% select(district_id, year,
                      p_ge1_major_year_mean, Lambda_import_year),
    by = c("district_id", "year")
  ) %>%
  filter(p_ge1_major_year_mean > NOISE_FLOOR) %>%
  mutate(log_p = log10(p_ge1_major_year_mean))

if (nrow(stat_df) > 10) {
  lm1 <- lm(log_p ~ T_season + I(T_season^2), data = stat_df)
  lm2 <- lm(log_p ~ T_season + I(T_season^2) + RH_season, data = stat_df)
  lm3 <- lm(log_p ~ T_season + I(T_season^2) + RH_season +
              log10(Lambda_import_year + 1), data = stat_df)

  # --- T_opt: from M2 (Lambda_import NOT included) ---
  b1_m2 <- coef(lm2)["T_season"]
  b2_m2 <- coef(lm2)["I(T_season^2)"]
  T_opt_m2 <- -b1_m2 / (2 * b2_m2)

  # --- For comparison, from M3 (collinearity artefact; DO NOT REPORT) ---
  b1_m3 <- coef(lm3)["T_season"]
  b2_m3 <- coef(lm3)["I(T_season^2)"]
  T_opt_m3 <- -b1_m3 / (2 * b2_m3)

  cat(sprintf("  T_opt (M2, reportable)               : %.1f °C\n", T_opt_m2))
  cat(sprintf("  T_opt (M3, Lambda included -- ARTEFACT) : %.1f °C  [do not report]\n",
              T_opt_m3))

  if (abs(T_opt_m2 - T_opt_m3) > 2)
    cat("  [NOTE] M2 and M3 optima differ by more than 2 C; this reflects the\n",
        "        collinearity between Lambda_import and temperature.\n")

  reg_comp <- tibble(
    Model = c("M1: T + T²", "M2: + RH", "M3: + Lambda_import (collinear)"),
    R2 = c(summary(lm1)$r.squared, summary(lm2)$r.squared,
           summary(lm3)$r.squared),
    adj_R2 = c(summary(lm1)$adj.r.squared, summary(lm2)$adj.r.squared,
               summary(lm3)$adj.r.squared),
    delta_R2 = c(NA, summary(lm2)$r.squared - summary(lm1)$r.squared,
                 summary(lm3)$r.squared - summary(lm2)$r.squared),
    ANOVA_p = c(NA, anova(lm1, lm2)$`Pr(>F)`[2],
                anova(lm2, lm3)$`Pr(>F)`[2]),
    T_opt = c(NA, round(T_opt_m2, 2), round(T_opt_m3, 2))
  ) %>%
    mutate(across(c(R2, adj_R2, delta_R2), ~round(.x, 4)),
           ANOVA_p = ifelse(is.na(ANOVA_p), "—",
                            format.pval(ANOVA_p, digits = 3)))

  write_csv(reg_comp, file.path(DIR_TBL, "tbl_regression_yearlyrisk.csv"))

  m2_coef <- broom::tidy(lm2, conf.int = TRUE) %>%
    transmute(
      Term = case_when(
        term == "(Intercept)"   ~ "Intercept",
        term == "T_season"      ~ "T_season",
        term == "I(T_season^2)" ~ "T_season²",
        term == "RH_season"     ~ "RH_season",
        TRUE                    ~ term
      ),
      Coefficient = round(estimate, 4),
      SE = round(std.error, 4),
      p = format.pval(p.value, digits = 3)
    )

  write_csv(m2_coef, file.path(DIR_TBL, "tbl_regression_coef_yearlyrisk.csv"))
  cat("  R² (M2):", round(summary(lm2)$r.squared, 3),
      "| n =", nrow(stat_df), "\n")
  cat("  [REMINDER] These tables do NOT produce the thesis's Section 6.10.\n")
  cat("             Canonical source: 6_10_pest_regression.R\n")
} else {
  cat("  Insufficient data -- regression skipped.\n")
}


## =========================================================
## 16) Mixed-Effects Model -- on MONTHLY MAJOR-OUTBREAK PROBABILITY (EXPLORATORY)
## ---------------------------------------------------------
## !!! THIS SECTION DOES NOT PRODUCE THE THESIS'S TABLES 6.10.2-6.10.6 !!!
##
## Differences:
##   - Dependent variable here: log10(p_month_major_mean); in the
##     thesis: log10(P_est).
##   - The ICC in this section comes from mm2, which INCLUDES log_lam
##     (Lambda_import); the thesis's ICC (0.839 / 0.843) comes from
##     LMM1, which does NOT include log_lam.
##   Together these two differences are the most likely source of two
##   different ICC/R2 sets being conflated in the thesis. Do NOT carry
##   the ICC value printed to the console over into the thesis text.
##
## v2 FIXES:
##   - v1 left the anova(), tidy(), VarCorr() lines bare; top-level
##     expressions inside source() are not auto-printed, so these lines
##     produced NO OUTPUT AT ALL. Wrapped in print().
##   - Results are now written to disk.
##   - This section was moved ABOVE the DONE banner.
##   - A reference model (mm0) WITHOUT log_lam was added, so it is
##     comparable with the thesis.
## =========================================================
cat(">>> 16) Mixed-effects model -- monthly p_major (EXPLORATORY) <<<\n")

if (requireNamespace("lme4", quietly = TRUE) &&
    requireNamespace("broom.mixed", quietly = TRUE)) {

  library(lme4)
  library(broom.mixed)

  stat_monthly <- monthly %>%
    filter(lambda_local_i1_mean > 0,
           p_month_major_mean > NOISE_FLOOR) %>%
    mutate(
      log_p   = log10(p_month_major_mean),
      log_lam = log10(lambda_import + 1e-10)
    )

  cat("  n (monthly observations) =", nrow(stat_monthly), "\n")

  # mm0: closest reference to the thesis's LMM1 SPECIFICATION (no log_lam)
  mm0 <- lmer(log_p ~ temp_c + I(temp_c^2) + rh + (1 | district_id),
              data = stat_monthly, REML = TRUE)
  # mm1/mm2: exploratory models with importation pressure added
  mm1 <- lmer(log_p ~ temp_c + I(temp_c^2) + rh + log_lam + (1 | district_id),
              data = stat_monthly, REML = TRUE)
  mm2 <- lmer(log_p ~ temp_c + I(temp_c^2) + rh + log_lam +
                (1 + temp_c | district_id),
              data = stat_monthly, REML = TRUE)

  icc_of <- function(mod) {
    vc <- as.data.frame(VarCorr(mod))
    vc$vcov[1] / (vc$vcov[1] + sigma(mod)^2)
  }
  T_opt_of <- function(mod) {
    fx <- fixef(mod)
    -fx["temp_c"] / (2 * fx["I(temp_c^2)"])
  }

  lmm_summary <- tibble(
    Model = c("mm0: T+T²+RH+(1|district)  [no Λ]",
              "mm1: + log Λ_import",
              "mm2: + random slope"),
    n     = nrow(stat_monthly),
    AIC   = c(AIC(mm0), AIC(mm1), AIC(mm2)),
    T_opt = c(T_opt_of(mm0), T_opt_of(mm1), T_opt_of(mm2)),
    ICC   = c(icc_of(mm0), icc_of(mm1), icc_of(mm2))
  ) %>%
    mutate(across(c(AIC, T_opt, ICC), ~round(.x, 4)))

  write_csv(lmm_summary, file.path(DIR_TBL, "tbl_lmm_monthly_pmajor.csv"))

  cat("  Mixed-model summary (dependent variable = log10(p_month,major)):\n")
  print(as.data.frame(lmm_summary), row.names = FALSE)

  cat("\n  Model comparison (refitted with ML):\n")
  print(anova(mm0, mm1, mm2))

  coef_tbl <- broom.mixed::tidy(mm1, conf.int = TRUE, effects = "fixed")
  write_csv(coef_tbl, file.path(DIR_TBL, "tbl_lmm_monthly_pmajor_coef.csv"))
  cat("\n  mm1 fixed effects:\n")
  print(as.data.frame(coef_tbl), row.names = FALSE)

  cat("\n  [WARNING] The ICC and T_opt values here are NOT the thesis's Table\n")
  cat("            6.10.4/6.10.6 values (different dependent variable; mm1/mm2\n")
  cat("            include Lambda_import). Do not carry these into the thesis.\n")
  cat("            Canonical source: 6_10_pest_regression.R\n")

} else {
  cat("  lme4 / broom.mixed not available -- mixed model skipped.\n")
}


## =========================================================
## DONE
## =========================================================
cat("\n", strrep("=", 55), "\n")
cat("DONE:", SSP_LABEL_EN, "\n")
cat("Figures:", DIR_FIG, "\n")
cat("Tables:", DIR_TBL, "\n")
cat(strrep("=", 55), "\n")
