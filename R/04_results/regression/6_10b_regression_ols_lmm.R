# =========================================================
# 6_10b_regression_ols_lmm.R   (COMPANION to 6_10_pest_regression.R)
# ---------------------------------------------------------
# Bolum 6.10'un EKSIK tablolarini uretir (duzeltilmis ana betikte YOK):
#   Tablo 6.10.1.1  Hiyerarsik OLS M1/M2 (R2, adj, dR2, ANOVA)   [yillik panel]
#   Tablo 6.10.1.2  OLS M1/M2 katsayilari                        [yillik panel]
#   Tablo 6.10.3    LMM1 sabit etki katsayilari                  [aylik panel]
#   Tablo 6.10.4    LMM marjinal/kosullu R2 (Nakagawa)           [aylik panel]
#   Tablo 6.10.2    LMM1 vs LMM2 uyum (AIC/BIC/logLik/chi2)      [KESIFSEL]
#
# ONEMLI:
#  * Lambda_import ACIKLAYICI DEGISKEN DEGILDIR (eski M3 = kollinearite
#    artefakti; T'yi anlamsizlastiriyordu -> KALDIRILDI).
#  * LMM2 (random slope) yalnizca 5 kume ile guvenilmez; KESIFSEL olarak
#    uretilir. Ana cikarim modeli LMM1'dir. Tercihen DOCX'te LMM2'yi
#    tek dipnota indirin.
#  * Kanonik (non-rep) aylik dosya varsa onu okur; yoksa _rep1000'e duser
#    (deger farki ~%1; DOCX ile birebir icin non-rep gerekir).
# Onkosul: init.R / paths.R + 00_results_setup.R sourced.
# =========================================================

suppressPackageStartupMessages({
  library(readr); library(dplyr); library(tidyr); library(here)
  library(lmerTest)      # LMM + Satterthwaite p
  library(performance)   # r2_nakagawa
  library(broom)
})

TARGET_SSP <- "ssp245"
FLOOR      <- 1e-15
LOG_FLOOR  <- log10(FLOOR)
TABLES_DIR <- here("outputs", "tables")
dir.create(TABLES_DIR, recursive = TRUE, showWarnings = FALSE)

DLAB <- c("TUR.40.25_1"="Kartal","TUR.10.4_1"="Hopa","TUR.59.4_1"="Fethiye",
          "TUR.81.6_1"="Zonguldak","TUR.39.3_1"="Egirdir")

topt_from_quad <- function(bT, bT2) as.numeric(-bT / (2 * bT2))

# ---- Aylik paneli ANA betikle AYNI kaynaktan oku (load_ssp_data = _rep1000) ----
#   Ana 6_10_pest_regression.R de load_ssp_data(ssp)$monthly kullanir ve
#   temp_c / rh / p_establishment_mean kolonlarini bekler (_rep1000 formati).
#   Iki betigin AYNI paneli okumasi 6.10 tablolarinin tutarliligi icin sarttir.
#   NOT (metodolojik): kanonik aylik P_est aslinda non-rep E[P_est]'tir
#   (horizon P_ufuk, isi haritalari orada). Regresyon _rep1000 g(E[lambda])
#   kullaniyor; tanimlayici post-hoc analiz icin savunulabilir ama iki estimand
#   farklidir. Ana betik boyle tasarlanmis; tutarlilik icin ona uyuluyor.
read_monthly <- function(s) {
  d <- load_ssp_data(s)
  monthly <- d$monthly

  p_col <- if ("p_establishment_mean" %in% names(monthly)) "p_establishment_mean"
           else if ("p_establishment" %in% names(monthly)) "p_establishment"
           else stop("P_est kolonu yok: ", paste(names(monthly), collapse = ", "))
  t_col <- if ("temp_c" %in% names(monthly)) "temp_c" else if ("T" %in% names(monthly)) "T"
           else stop("Sicaklik kolonu yok")
  rh_col <- if ("rh" %in% names(monthly)) "rh" else if ("RH" %in% names(monthly)) "RH"
           else stop("Nem kolonu yok")

  message("Panel kaynagi: load_ssp_data('", s, "') | P_est kolonu = ", p_col)
  tibble(
    district_id = monthly$district_id,
    year        = monthly$year,
    month       = monthly$month,
    T    = monthly[[t_col]],
    RH   = monthly[[rh_col]],
    Pest = monthly[[p_col]]
  )
}

mon <- read_monthly(TARGET_SSP)

# =========================================================
# (A) YILLIK PANEL  ->  Hiyerarsik OLS (Tablo 6.10.1.1 / 6.10.1.2)
#     Aktif aylar (Pest > FLOOR) uzerinden ilce-yil ortalamalari
# =========================================================
yearly <- mon %>%
  filter(Pest > FLOOR) %>%
  group_by(district_id, year) %>%
  summarise(T = mean(T), RH = mean(RH), logP = mean(log10(Pest)), .groups = "drop")

cat(sprintf("\n[OLS] Yillik panel n = %d ilce-yil\n", nrow(yearly)))

m1 <- lm(logP ~ T + I(T^2),      data = yearly)
m2 <- lm(logP ~ T + I(T^2) + RH, data = yearly)

r2  <- function(m) summary(m)$r.squared
ar2 <- function(m) summary(m)$adj.r.squared
aov12 <- anova(m1, m2)   # kismi F testi (M1 -> M2)

# Tablo 6.10.1.1
tbl_6101 <- tibble(
  Model   = c("M1: T + T\u00b2", "M2: M1 + RH"),
  R2      = round(c(r2(m1),  r2(m2)),  3),
  adj_R2  = round(c(ar2(m1), ar2(m2)), 3),
  dR2     = c(NA, round(r2(m2) - r2(m1), 3)),
  ANOVA_p = c(NA, signif(aov12$`Pr(>F)`[2], 3))
)
write_csv(tbl_6101, file.path(TABLES_DIR, "tbl_6_10_1_1_hierarchical_ols.csv"))
print(tbl_6101)

# Tablo 6.10.1.2  (M1 ve M2 katsayilari)
coef_tbl <- function(m, lbl) {
  s <- summary(m)$coefficients
  ci <- confint(m)
  tibble(
    Model = lbl,
    Terim = rownames(s),
    Katsayi = round(s[, "Estimate"], 4),
    SE = round(s[, "Std. Error"], 4),
    CI_alt = round(ci[, 1], 3), CI_ust = round(ci[, 2], 3),
    p = signif(s[, "Pr(>|t|)"], 3)
  )
}
tbl_6102 <- bind_rows(coef_tbl(m1, "M1"), coef_tbl(m2, "M2"))
write_csv(tbl_6102, file.path(TABLES_DIR, "tbl_6_10_1_2_ols_coef.csv"))
cat(sprintf("[OLS] T_opt (M2) = %.2f \u00b0C\n", topt_from_quad(coef(m2)[["T"]], coef(m2)[["I(T^2)"]])))

# =========================================================
# (B) AYLIK PANEL  ->  LMM1 (rastgele kesisim)  (Tablo 6.10.3 / 6.10.4)
# =========================================================
panel <- mon %>%
  filter(Pest > FLOOR) %>%
  transmute(district_id, logP = log10(Pest), T, RH)

cat(sprintf("\n[LMM] Aktif-ay panel n = %d\n", nrow(panel)))

lmm1 <- lmer(logP ~ T + I(T^2) + RH + (1 | district_id), data = panel, REML = TRUE)

# Tablo 6.10.3  (LMM1 sabit etkiler)
fe <- summary(lmm1)$coefficients
ci1 <- confint(lmm1, method = "Wald")
ci1 <- ci1[rownames(fe), ]
tbl_6103 <- tibble(
  Terim = rownames(fe),
  beta  = round(fe[, "Estimate"], 4),
  SE    = round(fe[, "Std. Error"], 4),
  t     = round(fe[, "t value"], 1),
  p     = signif(fe[, "Pr(>|t|)"], 3),
  CI_alt = round(ci1[, 1], 3), CI_ust = round(ci1[, 2], 3)
)
write_csv(tbl_6103, file.path(TABLES_DIR, "tbl_6_10_3_lmm1_fixef.csv"))
print(tbl_6103)
cat(sprintf("[LMM1] T_opt = %.2f \u00b0C\n",
            topt_from_quad(fe[["T","Estimate"]], fe[["I(T^2)","Estimate"]])))

# ICC + Nakagawa R2 (LMM1)
icc1 <- performance::icc(lmm1)$ICC_adjusted
r2_1 <- performance::r2_nakagawa(lmm1)
cat(sprintf("[LMM1] ICC = %.3f | R2_marj = %.3f | R2_kos = %.3f\n",
            icc1, r2_1$R2_marginal, r2_1$R2_conditional))

# =========================================================
# (C) LMM2 (rastgele egim) - KESIFSEL  (Tablo 6.10.2 / 6.10.4 LMM2 satiri)
#     5 kume ile guvenilmez; yalnizca uyum karsilastirmasi icin.
# =========================================================
lmm2 <- lmer(logP ~ T + I(T^2) + RH + (T | district_id), data = panel, REML = FALSE)
lmm1_ml <- update(lmm1, REML = FALSE)   # LRT icin ML

lrt <- anova(lmm1_ml, lmm2)   # chi2, df, p
r2_2 <- performance::r2_nakagawa(lmm2)

# Tablo 6.10.2 (uyum)
tbl_6102fit <- tibble(
  Model  = c("LMM1: (1|ilce)", "LMM2: + (T|ilce)"),
  AIC    = round(c(AIC(lmm1_ml), AIC(lmm2)), 1),
  BIC    = round(c(BIC(lmm1_ml), BIC(lmm2)), 1),
  logLik = round(c(as.numeric(logLik(lmm1_ml)), as.numeric(logLik(lmm2))), 1),
  dAIC   = c(NA, round(AIC(lmm1_ml) - AIC(lmm2), 1)),
  chi2   = c(NA, round(lrt$Chisq[2], 1)),
  p      = c(NA, signif(lrt$`Pr(>Chisq)`[2], 3))
)
write_csv(tbl_6102fit, file.path(TABLES_DIR, "tbl_6_10_2_lmm_fit.csv"))
print(tbl_6102fit)

# Tablo 6.10.4  (marjinal/kosullu R2)
tbl_6104 <- tibble(
  Model       = c("LMM1: rastgele kesisim", "LMM2: kesisim + egim (kesifsel)"),
  R2_marjinal = round(c(r2_1$R2_marginal,    r2_2$R2_marginal),    3),
  R2_kosullu  = round(c(r2_1$R2_conditional, r2_2$R2_conditional), 3)
)
write_csv(tbl_6104, file.path(TABLES_DIR, "tbl_6_10_4_lmm_r2.csv"))
print(tbl_6104)

cat("\n\u2713 Eksik 6.10 tablolari uretildi (outputs/tables/):\n",
    "  tbl_6_10_1_1_hierarchical_ols.csv, tbl_6_10_1_2_ols_coef.csv,\n",
    "  tbl_6_10_3_lmm1_fixef.csv, tbl_6_10_4_lmm_r2.csv, tbl_6_10_2_lmm_fit.csv\n")
