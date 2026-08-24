# =============================================================================
# kvpd_sensitivity_check.R                                    [SURUM 3 - NIHAI]
# -----------------------------------------------------------------------------
# AMAC
#   k_vpd = 0.3 / 0.5 / 0.8 kPa^-1 icin yerlesme olasiligini gercek ilce-ay
#   iklim yorungelerinden hesaplar; risk hiyerarsisinin k_vpd'ye duyarliligini
#   ve maks P_est / ufuk riski P_horizon degerlerini raporlar.
#
# -----------------------------------------------------------------------------
# SURUM 3 DUZELTMELERI
#
#   [D1] POISSON INCELTMESI (surum 2'de yapildi, korunuyor)
#        p_ay = 1 - exp(-lambda_import * P_est)   — Bernoulli carpimi DEGIL.
#
#   [D2] TASMA GUVENLI P_est (surum 2, korunuyor)
#        q(tau) kapali formu R0 << 1 iken R0^-tau -> Inf uretip NaN veriyordu.
#        Cebirsel esdeger iki dalli forma gecildi.
#
#   [D3] R0 = 1 SINIR DURUMU (surum 2, korunuyor):  P_est = 1/tau.
#
#   [D4] CAPALAMA — YENI VE KRITIK
#        Bu betigin ic sdlog(T) kalibrasyonu ana hattinkiyle tam ayni degildir
#        (bu betik: aegypti 0.465 / albopictus 0.434 @25C; ana hat ~0.487/0.453).
#        Fark kucuk gorunse de P_est ~ R0^tau (tau=30) uzerinden ustel olarak
#        buyur ve dusuk riskli ilcelerde 5-6 kat sapma yaratir.
#
#        Cozum: mutlak seviyeyi yeniden hesaplamak yerine k_vpd'nin GORELI
#        etkisini hesaplayip ana hattin sakladigi P_est'e uygulamak:
#
#            P_est(k) = P_est_kanonik * [ P_est_yeniden(k) / P_est_yeniden(0.5) ]
#
#        Boylece k=0.5 sutunu ana hat ile TAM ortusur; k=0.3 ve k=0.8 sutunlari
#        ana hattin kalibrasyonunu devralir. sdlog farki oran icinde sadelesir.
#        MODE = "anchored" (varsayilan) bunu yapar; MODE = "recompute" saf
#        yeniden hesaplamayi (tani amacli) verir.
#
#   [D5] na.rm eksikleri; which.max NaN korumasi.
#   [D6] Girdi dondurulmus kanonik ciktidan (outputs/_canonical/).
#   [D7] Ic Monte Carlo vektorlestirildi.
#   [D8] Hukum olcutu medyan yerine MAKSIMUM |log10 fark| uzerinden.
#
# -----------------------------------------------------------------------------
# CIKTI
#   outputs/tables/tbl_kvpd_sensitivity.csv
#   outputs/tables/tbl_kvpd_reference_check.csv
#   outputs/tables/tbl_kvpd_canonical_check.csv
#   outputs/tables/tbl_kvpd_hierarchy.csv
#
# MEKANISTIK FORMULLER (parameter_functions.R / ctmc_spark.R ile ayni)
#   a(T)=Briere; EIP(T)=1/Briere_dev; lf(T)=max(quad_unimodal, 0.25)
#   VPD = max(0.6108*exp(17.27T/(T+237.3))*(1-RH/100), 0)
#   mu_v = max((1/lf)*exp(k_vpd*(VPD-VPD_ref)), 1e-6)
#   lambda1 = m*a^2*beta_vh*beta_hv*exp(-mu_v*EIP)/mu_v ;  R0 = lambda1/gamma
#   P_est = 1 - q(tau), tau = 30 esikli gambler's ruin
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(purrr); library(readr); library(here)
})

# ---- Yapilandirma -----------------------------------------------------------
MODE <- "anchored"      # "anchored" (tez icin) | "recompute" (tani amacli)

SSP_SCENARIOS <- c("ssp126", "ssp245", "ssp585")
SSP_LABELS    <- c(ssp126 = "SSP1-2.6", ssp245 = "SSP2-4.5", ssp585 = "SSP5-8.5")
K_VPD_VALUES  <- c(0.3, 0.5, 0.8)
K_BASE        <- 0.5                       # baz deger (Schmidt 2018)
MONTHLY_FILE  <- "ctmc_spark_monthly_2025_2075_rep1000.rds"
HORIZON_FILE  <- "ctmc_spark_horizon_2025_2075_rep1000.csv"

USE_JENSEN <- TRUE
N_MC       <- 2000
SEED       <- 12345

BETA_VH <- 0.30; BETA_HV <- 0.33; GAMMA <- 0.20; M_RATIO <- 1.0
TAU <- 30L; LF_FLOOR <- 0.25; VPD_REF <- 1.0

DISTRICT_ORDER  <- c("TUR.40.25_1","TUR.10.4_1","TUR.81.6_1","TUR.59.4_1","TUR.39.3_1")
DISTRICT_LABELS <- c("TUR.40.25_1" = "Kartal",   "TUR.10.4_1" = "Hopa",
                     "TUR.81.6_1"  = "Zonguldak","TUR.59.4_1" = "Fethiye",
                     "TUR.39.3_1"  = "Egirdir")

TRAITS <- list(
  aegypti = list(
    a   = c(c = 2.71e-4, T0 = 14.67, Tm = 41.00),
    dev = c(c = 1.04e-4, T0 = 11.50, Tm = 38.97),
    lf  = c(c = 1.48e-1, T0 = 9.16,  Tm = 37.73)
  ),
  albopictus = list(
    a   = c(c = 1.93e-4, T0 = 10.25, Tm = 38.32),
    dev = c(c = 1.09e-4, T0 = 10.39, Tm = 43.05),
    lf  = c(c = 1.43e-1, T0 = 6.24,  Tm = 38.25)
  )
)

# ---- Termal performans ilkelleri --------------------------------------------
briere <- function(T, c, T0, Tm) {
  out <- numeric(length(T)); ok <- is.finite(T) & T > T0 & T < Tm
  out[ok] <- c * T[ok] * (T[ok] - T0) * sqrt(Tm - T[ok])
  as.numeric(pmax(out, 0))
}
quadratic_unimodal <- function(T, c, T0, Tm) {
  out <- numeric(length(T)); ok <- is.finite(T) & T > T0 & T < Tm
  out[ok] <- -c * (T[ok] - T0) * (T[ok] - Tm)
  as.numeric(pmax(out, 0))
}
vpd_kpa <- function(T, RH) {
  es <- 0.6108 * exp(17.27 * T / (T + 237.3))
  as.numeric(pmax(es - es * RH / 100, 0))
}

# ---- EIP log-normal sdlog(T) kalibrasyonu -----------------------------------
# NOT: capalama modunda bu kalibrasyonun ana hatla birebir ayni olmasi
# GEREKMEZ; oran icinde sadelesir. "recompute" modunda ise onemlidir.
.calibrate_sdlog <- function(dev_par, Tref = 27.5) {
  eip_mean <- function(T) {
    r <- briere(T, dev_par["c"], dev_par["T0"], dev_par["Tm"])
    ifelse(r > 0, 1 / r, Inf)
  }
  anchors <- list(list(T = 25, q5 = 5, q95 = 33), list(T = 30, q5 = 2, q95 = 15))
  z05 <- qnorm(0.05); z95 <- qnorm(0.95)
  qfun <- function(mean, s, p)
    if (!is.finite(mean) || mean <= 0) Inf else exp(log(mean) - 0.5 * s^2 + s * qnorm(p))
  s0  <- function(q5, q95) log(q95 / q5) / (z95 - z05)
  s25 <- s0(5, 33); s30 <- s0(2, 15)
  beta0  <- (log(s30) - log(s25)) / (30 - 25)
  alpha0 <- log(s25) - beta0 * (25 - Tref)
  obj <- function(th) {
    a <- th[1]; b <- th[2]; ss <- 0
    for (an in anchors) {
      s <- exp(a + b * (an$T - Tref)); mE <- eip_mean(an$T)
      ss <- ss + (log(qfun(mE, s, 0.05) / an$q5))^2 +
                 (log(qfun(mE, s, 0.95) / an$q95))^2
    }
    ss
  }
  fit <- optim(c(alpha0, beta0), obj, method = "BFGS",
               control = list(reltol = 1e-10, maxit = 200))
  list(alpha = fit$par[1], beta = fit$par[2], Tref = Tref)
}

# ---- [D2][D3] Tasma guvenli P_est (vektorlestirilmis) -----------------------
# P_est = 1 - q(tau) = (1 - R0^-1)/(1 - R0^-tau)
#   R0 > 1 : R0^-tau  -> 0 (alt tasma, zararsiz)
#   R0 < 1 : pay/paydayi R0^tau ile carp -> (R0^tau - R0^(tau-1))/(R0^tau - 1)
#   R0 = 1 : P_est = 1/tau
p_est_from_R0 <- function(R0) {
  p   <- numeric(length(R0))
  bad <- !is.finite(R0) | R0 <= 0
  p[bad] <- 0

  one <- !bad & abs(R0 - 1) < 1e-10
  p[one] <- 1 / TAU

  hi <- !bad & !one & R0 > 1
  if (any(hi)) p[hi] <- (1 - 1 / R0[hi]) / (1 - exp(-TAU * log(R0[hi])))

  lo <- !bad & !one & R0 < 1
  if (any(lo)) {
    lr  <- log(R0[lo])
    rt  <- exp(TAU * lr)
    rt1 <- exp((TAU - 1) * lr)
    p[lo] <- (rt - rt1) / (rt - 1)
  }

  p[!is.finite(p)] <- 0
  pmin(pmax(p, 0), 1)
}

.lambda1 <- function(aT, muv, eip) {
  surv <- ifelse(is.finite(eip), exp(-muv * eip), 0)
  M_RATIO * aT^2 * BETA_VH * BETA_HV * surv / muv
}

p_est_one <- function(T, RH, species, k_vpd, use_jensen, n_mc, sdlog_fit) {
  tr  <- TRAITS[[species]]
  aT  <- briere(T, tr$a["c"], tr$a["T0"], tr$a["Tm"])
  lf  <- max(quadratic_unimodal(T, tr$lf["c"], tr$lf["T0"], tr$lf["Tm"]), LF_FLOOR)
  muv <- max((1 / lf) * exp(k_vpd * (vpd_kpa(T, RH) - VPD_REF)), 1e-6)
  dev <- briere(T, tr$dev["c"], tr$dev["T0"], tr$dev["Tm"])
  eip_mean <- if (dev > 0) 1 / dev else Inf

  if (!is.finite(aT) || aT <= 0 || !is.finite(eip_mean)) return(0)

  if (!use_jensen)
    return(p_est_from_R0(.lambda1(aT, muv, eip_mean) / GAMMA))

  cf <- sdlog_fit[[species]]
  s  <- exp(cf$alpha + cf$beta * (T - cf$Tref))
  eip_draws <- rlnorm(n_mc, meanlog = log(eip_mean) - 0.5 * s^2, sdlog = s)
  mean(p_est_from_R0(.lambda1(aT, muv, eip_draws) / GAMMA))   # [D7]
}

# ---- Yol cozumleme [D6] ------------------------------------------------------
monthly_path <- function(ssp) {
  p <- here::here("outputs", "_canonical", ssp, "simulation", MONTHLY_FILE)
  if (!file.exists(p))
    stop("Kanonik aylik dosya bulunamadi: ", p,
         "\nOnce 02_freeze_canonical.R ve 02c_freeze_remaining.R kosulmali.")
  p
}
horizon_path <- function(ssp)
  here::here("outputs", "_canonical", ssp, "simulation", HORIZON_FILE)

# =============================================================================
# BOLUM 1 — k_vpd duyarliligi
# =============================================================================
set.seed(SEED)
sdlog_fit <- lapply(TRAITS, function(tr) .calibrate_sdlog(tr$dev))

cat("=== sdlog(T) kalibrasyonu (bu betik) ===\n")
for (sp in names(sdlog_fit)) {
  cf <- sdlog_fit[[sp]]
  cat(sprintf("  %-11s sdlog(25)=%.4f  sdlog(30)=%.4f\n", sp,
      exp(cf$alpha + cf$beta * (25 - cf$Tref)),
      exp(cf$alpha + cf$beta * (30 - cf$Tref))))
}
cat(sprintf("  MOD = %s\n", MODE))
if (MODE == "anchored")
  cat(sprintf("  -> k=%.1f sutunu ana hat P_est'ine capalanir; k_vpd etkisi oran olarak uygulanir.\n",
              K_BASE))

sens_rows <- list(); calib_rows <- list()

for (ssp in SSP_SCENARIOS) {
  df <- readRDS(monthly_path(ssp))

  req <- c("district_id", "species", "temp_c", "rh", "lambda_import")
  if (!all(req %in% names(df)))
    stop(sprintf("[%s] eksik sutun: %s", ssp,
                 paste(setdiff(req, names(df)), collapse = ", ")))
  if (MODE == "anchored" && !"p_establishment_mean" %in% names(df))
    stop(sprintf("[%s] capalama icin p_establishment_mean gerekli.", ssp))

  message(sprintf("  [%s] satir=%d", ssp, nrow(df)))

  # Her k icin ham (yeniden hesaplanmis) P_est
  pe_raw <- lapply(K_VPD_VALUES, function(k)
    vapply(seq_len(nrow(df)), function(i)
      p_est_one(df$temp_c[i], df$rh[i], df$species[i], k, USE_JENSEN, N_MC, sdlog_fit),
      numeric(1)))
  names(pe_raw) <- as.character(K_VPD_VALUES)

  pe_base_raw <- pe_raw[[as.character(K_BASE)]]
  pe_stored   <- if ("p_establishment_mean" %in% names(df)) df$p_establishment_mean else NULL

  # --- [D4] CAPALAMA ---------------------------------------------------------
  pe_use <- lapply(K_VPD_VALUES, function(k) {
    raw <- pe_raw[[as.character(k)]]
    if (MODE != "anchored") return(raw)
    oran <- ifelse(pe_base_raw > 0, raw / pe_base_raw, 0)   # 0/0 -> 0
    pmin(pmax(pe_stored * oran, 0), 1)
  })
  names(pe_use) <- as.character(K_VPD_VALUES)

  for (k in K_VPD_VALUES) {
    pe  <- pe_use[[as.character(k)]]
    if (anyNA(pe))
      warning(sprintf("[%s k=%.1f] %d NA — incelenmeli", ssp, k, sum(is.na(pe))),
              call. = FALSE)

    pmm <- 1 - exp(-df$lambda_import * pe)                   # [D1]

    sens_rows[[paste(ssp, k)]] <- tibble(
        district_id = df$district_id, pe = pe, pmm = pmm) %>%
      group_by(district_id) %>%
      summarise(max_p_est = max(pe, na.rm = TRUE),           # [D5]
                P_horizon = 1 - prod(1 - pmm[is.finite(pmm)]),
                .groups = "drop") %>%
      mutate(ssp = ssp, ssp_label = unname(SSP_LABELS[ssp]), k_vpd = k,
             district = dplyr::recode(district_id, !!!DISTRICT_LABELS))
  }

  # Ham yeniden hesaplama ile saklanan P_est arasindaki uyum (tani)
  if (!is.null(pe_stored)) {
    act <- is.finite(pe_stored) & pe_stored > 1e-12
    if (any(act)) calib_rows[[ssp]] <- tibble(
      ssp        = ssp,
      n_active   = sum(act),
      corr_log10 = suppressWarnings(cor(log10(pmax(pe_base_raw[act], 1e-300)),
                                        log10(pe_stored[act]))),
      medyan_oran = median(pe_base_raw[act] / pe_stored[act]),
      my_max     = max(pe_base_raw, na.rm = TRUE),
      stored_max = max(pe_stored, na.rm = TRUE))
  }
}

sens <- bind_rows(sens_rows) %>%
  mutate(ssp = factor(ssp, levels = SSP_SCENARIOS),
         district_id = factor(district_id, levels = DISTRICT_ORDER)) %>%
  arrange(ssp, district_id, k_vpd) %>%
  mutate(ssp = as.character(ssp), district_id = as.character(district_id))

# ---- Hiyerarsi --------------------------------------------------------------
hier_detail <- sens %>%
  group_by(ssp_label, k_vpd) %>%
  arrange(desc(P_horizon), .by_group = TRUE) %>%
  mutate(sira = row_number()) %>%
  ungroup() %>%
  select(Senaryo = ssp_label, k_vpd, sira, Ilce = district, P_horizon)

hierarchy_ok <- sens %>%
  group_by(ssp, k_vpd) %>%
  arrange(desc(P_horizon), .by_group = TRUE) %>%
  summarise(order = paste(district, collapse = ">"), .groups = "drop") %>%
  group_by(ssp) %>%
  summarise(preserved = n_distinct(order) == 1,
            orders = paste(unique(order), collapse = "  |  "), .groups = "drop")

# ---- Yaz --------------------------------------------------------------------
out_dir <- here::here("outputs", "tables")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

write_csv(sens %>% transmute(Senaryo = ssp_label, Ilce = district, k_vpd = k_vpd,
                             `maks P_est` = max_p_est, P_horizon = P_horizon,
                             mod = MODE),
          file.path(out_dir, "tbl_kvpd_sensitivity.csv"))
write_csv(hier_detail, file.path(out_dir, "tbl_kvpd_hierarchy.csv"))

fmt <- function(x) formatC(x, format = "e", digits = 3)

cat("\n=========== k_vpd DUYARLILIGI — P_horizon ===========\n")
sens %>% select(ssp_label, district, k_vpd, P_horizon) %>%
  pivot_wider(names_from = k_vpd, values_from = P_horizon, names_prefix = "k=") %>%
  mutate(across(starts_with("k="), fmt)) %>%
  as.data.frame() %>% print(row.names = FALSE)

cat("\n----------- maks P_est -----------\n")
sens %>% select(ssp_label, district, k_vpd, max_p_est) %>%
  pivot_wider(names_from = k_vpd, values_from = max_p_est, names_prefix = "k=") %>%
  mutate(across(starts_with("k="), ~ formatC(.x, format = "f", digits = 4))) %>%
  as.data.frame() %>% print(row.names = FALSE)

cat("\n----------- HIYERARSI KORUNUMU -----------\n")
for (i in seq_len(nrow(hierarchy_ok)))
  cat(sprintf("  %s: %s\n", SSP_LABELS[hierarchy_ok$ssp[i]],
      if (hierarchy_ok$preserved[i]) "TUM k_vpd degerlerinde KORUNUYOR"
      else paste0("DEGISIYOR -> ", hierarchy_ok$orders[i])))

if (length(calib_rows) > 0) {
  cat("\n--- TANI: ham yeniden hesaplama vs saklanan P_est (capalama ONCESI) ---\n")
  cb <- bind_rows(calib_rows)
  for (i in seq_len(nrow(cb)))
    cat(sprintf("  %s: n=%d corr_log10=%.4f medyan_oran=%.3f my_max=%.4f stored_max=%.4f\n",
        SSP_LABELS[cb$ssp[i]], cb$n_active[i], cb$corr_log10[i],
        cb$medyan_oran[i], cb$my_max[i], cb$stored_max[i]))
  cat("  (medyan_oran 1'den uzaksa sdlog kalibrasyonlari ayrisiyor demektir;\n",
      "   capalama modu bu farki oran icinde sadelestirir.)\n", sep = "")
}

# =============================================================================
# BOLUM 1b — KANONIK CAPRAZ KONTROL [D8]
# =============================================================================
cat("\n=========== KANONIK CAPRAZ KONTROL (k=", K_BASE, " vs ana hat) ===========\n",
    sep = "")

canon <- map_dfr(SSP_SCENARIOS, function(s) {
  f <- horizon_path(s)
  if (!file.exists(f)) return(tibble())
  d  <- read_csv(f, show_col_types = FALSE)
  pc <- if ("p_ge1_major_mean" %in% names(d)) "p_ge1_major_mean" else "p_ge1_major"
  tibble(ssp = s, district_id = d$district_id, P_canonical = d[[pc]])
})

if (nrow(canon) > 0) {
  chk <- sens %>% filter(abs(k_vpd - K_BASE) < 1e-9) %>%
    select(ssp, ssp_label, district_id, district, P_kvpd = P_horizon) %>%
    left_join(canon, by = c("ssp", "district_id")) %>%
    mutate(oran = P_kvpd / P_canonical,
           log10_fark = log10(pmax(P_kvpd, 1e-300)) - log10(pmax(P_canonical, 1e-300)))

  write_csv(chk, file.path(out_dir, "tbl_kvpd_canonical_check.csv"))

  chk %>% transmute(Senaryo = ssp_label, Ilce = district,
                    `k_vpd` = fmt(P_kvpd), `ana hat` = fmt(P_canonical),
                    oran = round(oran, 4), `log10 fark` = round(log10_fark, 4)) %>%
    as.data.frame() %>% print(row.names = FALSE)

  mx <- max(abs(chk$log10_fark), na.rm = TRUE)
  md <- median(abs(chk$log10_fark), na.rm = TRUE)
  cat(sprintf("\n  Maks |log10 fark| = %.5f (%.3f kat)  |  medyan = %.5f\n",
              mx, 10^mx, md))
  if (mx < 0.02) {
    cat("  DURUM: TUTARLI — k_vpd tablosu ana hat ile hizali.\n")
  } else if (mx < 0.15) {
    cat("  DURUM: Sinirda. Tez metninde sapma belirtilmeli.\n")
  } else {
    cat("  DURUM: !! ILCE BAZLI ANLAMLI SAPMA.\n",
        "         MODE='anchored' ise capalama beklendigi gibi calismamis;\n",
        "         pe_stored ve pe_base_raw hizalanmasini kontrol et.\n", sep = "")
  }
}

# =============================================================================
# BOLUM 2 — Referans kosul kontrolu (RH = 75%)
# =============================================================================
cat("\n=========== REFERANS KOSUL (RH = 75%, deterministik) ===========\n")
Tgrid <- seq(0, 45, by = 0.02)
ref <- map_dfr(names(TRAITS), function(sp) {
  map_dfr(K_VPD_VALUES, function(k) {
    pe <- vapply(Tgrid, function(T)
      p_est_one(T, 75, sp, k, use_jensen = FALSE, N_MC, sdlog_fit), numeric(1))
    ok <- is.finite(pe)
    tibble(species = sp, k_vpd = k,
           max_p_est = if (any(ok)) max(pe[ok]) else NA_real_,          # [D5]
           T_at_max  = if (any(ok)) Tgrid[which.max(replace(pe, !ok, -Inf))]
                       else NA_real_)
  })
})
write_csv(ref, file.path(out_dir, "tbl_kvpd_reference_check.csv"))

ref %>% select(species, k_vpd, max_p_est) %>%
  pivot_wider(names_from = k_vpd, values_from = max_p_est, names_prefix = "k=") %>%
  mutate(across(starts_with("k="),
                ~ ifelse(is.na(.x), "NA", sprintf("%.1f%%", .x * 100)))) %>%
  as.data.frame() %>% print(row.names = FALSE)

cat("\n--- maks P_est'in gozlendigi sicaklik (C) ---\n")
ref %>% select(species, k_vpd, T_at_max) %>%
  pivot_wider(names_from = k_vpd, values_from = T_at_max, names_prefix = "k=") %>%
  as.data.frame() %>% print(row.names = FALSE)

cat("\nNOT: S2 File'daki 38.3 / 38.3 / 38.8% degerleri bu hesapla dogrulanmiyor.\n",
    "Yukaridaki tur-ayrimli degerler kullanilmali veya iddia metinden cikarilmalidir.\n",
    sep = "")

cat("\nTamamlandi (MOD = ", MODE, "). Tablolar: ", out_dir, "\n", sep = "")
