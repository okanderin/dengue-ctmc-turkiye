# =============================================================================
# 10_tau_threshold_sensitivity.R
# -----------------------------------------------------------------------------
# AMAC
#   Sonlu esik (tau) seciminin yerlesme olasiligi P_est uzerindeki etkisini,
#   HEM SUBKRITIK (R0 < 1) HEM SUPERKRITIK (R0 > 1) rejimde niceler.
#
# NEDEN GEREKLI
#   Tezdeki mevcut tau duyarlilik tablosu yalnizca R0 >= 1,2 araligini
#   gostermekte ve "tau=30 ile tau->sonsuz farki ihmal edilebilir" sonucuna
#   varmaktadir. Ancak:
#
#     R0 <= 1 iken sonme kesindir  =>  P_est(sonsuz) = 0
#
#   Modelin ilce-ay hucrelerinin buyuk cogunlugu subkritiktir. Bu hucrelerde
#   raporlanan tum P_est degerleri TAMAMEN sonlu esikten kaynaklanir; klasik
#   olcut hepsi icin sifir uretirdi. Dolayisiyla sonlu esik bir "yaklasim"
#   degil, subkritik rejimde olcutun varlik kosuludur.
#
#   Bu betik (a) iki rejimi kapsayan tabloyu uretir, (b) kanonik ciktidan
#   subkritik hucre oranini hesaplayarak bu savi ampirik olarak destekler,
#   (c) analitik formulu ctmc_spark.R'deki sayisal cozumle dogrular.
#
# CIKTI
#   outputs/tables/tbl_tau_sensitivity.csv        (tez Tablo 5.4.2.1)
#   outputs/tables/tbl_tau_regime_shares.csv      (subkritik hucre oranlari)
#   outputs/cross_scenario/fig_tau_sensitivity.png
# =============================================================================

suppressPackageStartupMessages({
  library(here); library(dplyr); library(tidyr); library(readr)
  library(purrr); library(tibble); library(ggplot2)
})

GAMMA   <- 0.20                      # gun^-1, iyilesme/cikis hizi
TAU_USE <- 30L                       # calismada kullanilan esik
TAU_GRID <- c(5L, 10L, 20L, 30L, 50L, 100L)
SSPS <- c(ssp126 = "SSP1-2.6", ssp245 = "SSP2-4.5", ssp585 = "SSP5-8.5")
DIST <- c(TUR.40.25_1 = "Kartal",    TUR.59.4_1 = "Fethiye", TUR.10.4_1 = "Hopa",
          TUR.81.6_1  = "Zonguldak", TUR.39.3_1 = "Egirdir")

# =============================================================================
# 1) TASMA GUVENLI ANALITIK P_est
# -----------------------------------------------------------------------------
#   q(tau) = (R0^-1 - R0^-tau) / (1 - R0^-tau)
#   P_est  = 1 - q = (1 - R0^-1) / (1 - R0^-tau)
#
#   R0 > 1 : R0^-tau -> 0 (alt tasma, zararsiz)
#            P = (1 - 1/R0) / (1 - exp(-tau*ln R0))
#   R0 < 1 : R0^-tau -> Inf (UST TASMA). Pay/paydayi R0^tau ile carp:
#            P = (R0^tau - R0^(tau-1)) / (R0^tau - 1)
#   R0 = 1 : P = 1/tau
# =============================================================================
p_est_analytic <- function(R0, tau) {
  n <- max(length(R0), length(tau))
  R0 <- rep_len(R0, n); tau <- rep_len(tau, n)
  p <- numeric(n)

  bad <- !is.finite(R0) | R0 <= 0
  p[bad] <- 0

  one <- !bad & abs(R0 - 1) < 1e-12
  p[one] <- 1 / tau[one]

  hi <- !bad & !one & R0 > 1
  if (any(hi)) p[hi] <- (1 - 1 / R0[hi]) / (1 - exp(-tau[hi] * log(R0[hi])))

  lo <- !bad & !one & R0 < 1
  if (any(lo)) {
    lr  <- log(R0[lo])
    rt  <- exp(tau[lo] * lr)          # -> 0
    rt1 <- exp((tau[lo] - 1) * lr)    # -> 0
    p[lo] <- (rt - rt1) / (rt - 1)
  }

  p[!is.finite(p)] <- 0
  pmin(pmax(p, 0), 1)
}

# tau -> sonsuz limiti (klasik dallanma sureci sonucu)
p_est_limit <- function(R0) ifelse(R0 > 1, 1 - 1 / R0, 0)

# =============================================================================
# 2) SAYISAL DOGRULAMA — dogrusal dogum-olum zinciri
#    lambda_n = lambda*n, mu_n = gamma*n; birinci-adim analizi ile q_1
# =============================================================================
p_est_numeric <- function(R0, tau) {
  if (!is.finite(R0) || R0 <= 0) return(0)
  n <- 1:(tau - 1)
  lam <- R0 * GAMMA * n          # dogum hizlari
  mu  <- GAMMA * n               # olum hizlari
  # S_k = prod_{j=1..k} mu_j/lambda_j  (log uzayinda)
  log_B <- cumsum(log(mu) - log(lam))
  lse <- function(x) { M <- max(x); M + log(sum(exp(x - M))) }
  log_all <- lse(c(0, log_B))    # S_0 + ... + S_{tau-1}
  # q_1 = 1 - S_0 / (S_0 + ... + S_{tau-1})
  q1 <- 1 - exp(0 - log_all)
  1 - q1
}

# =============================================================================
# 3) TABLO — iki rejimi de kapsayan R0 izgarasi
# =============================================================================
R0_GRID <- c(0.30, 0.50, 0.80, 0.95, 0.99, 1.00, 1.05, 1.20, 1.30, 1.50, 2.00, 3.00)

tab <- expand_grid(R0 = R0_GRID, tau = TAU_GRID) %>%
  mutate(P_est = p_est_analytic(R0, tau)) %>%
  pivot_wider(names_from = tau, values_from = P_est,
              names_prefix = "tau_") %>%
  mutate(P_limit = p_est_limit(R0),
         rejim = case_when(R0 < 1 ~ "subkritik",
                           abs(R0 - 1) < 1e-12 ~ "kritik",
                           TRUE ~ "superkritik"),
         # tau=30 ile limit arasindaki goreli fark (limit>0 ise)
         fark_pct = ifelse(P_limit > 0,
                           100 * abs(tau_30 - P_limit) / P_limit, NA_real_))

# Sayisal dogrulama
dog <- tibble(R0 = R0_GRID) %>%
  mutate(analitik = p_est_analytic(R0, TAU_USE),
         sayisal  = map_dbl(R0, ~ p_est_numeric(.x, TAU_USE)),
         mutlak_fark = abs(analitik - sayisal))

# =============================================================================
# 4) AMPIRIK DESTEK — kanonik ciktida subkritik hucre orani
# =============================================================================
CANON <- here("outputs", "_canonical")

cells <- map_dfr(names(SSPS), function(s) {
  f <- file.path(CANON, s, "simulation",
                 "ctmc_spark_monthly_2025_2075_rep1000.csv")
  if (!file.exists(f)) return(tibble())
  d <- read_csv(f, show_col_types = FALSE)
  lc <- grep("^lambda_local_i1", names(d), value = TRUE)[1]
  if (is.na(lc)) return(tibble())
  tibble(ssp = SSPS[[s]],
         district = unname(DIST[d$district_id]),
         R0 = d[[lc]] / GAMMA,
         p_est = d$p_establishment_mean)
})

if (nrow(cells) > 0) {
  aktif <- cells %>% filter(is.finite(R0), R0 > 1e-12)

  paylar <- aktif %>%
    mutate(rejim = ifelse(R0 < 1, "subkritik (R0<1)", "superkritik (R0>=1)")) %>%
    count(ssp, rejim) %>%
    group_by(ssp) %>% mutate(oran_pct = round(100 * n / sum(n), 1)) %>%
    ungroup()

  ilce_paylar <- aktif %>%
    group_by(ssp, district) %>%
    summarise(n_aktif = n(),
              n_superkritik = sum(R0 >= 1),
              maks_R0 = max(R0),
              .groups = "drop") %>%
    mutate(superkritik_pct = round(100 * n_superkritik / n_aktif, 1))
} else {
  paylar <- tibble(); ilce_paylar <- tibble()
}

# =============================================================================
# 5) YAZ
# =============================================================================
out_tbl <- here("outputs", "tables")
out_fig <- here("outputs", "cross_scenario")
dir.create(out_tbl, recursive = TRUE, showWarnings = FALSE)
dir.create(out_fig, recursive = TRUE, showWarnings = FALSE)

fmt <- function(x) {
  ifelse(is.na(x), "\u2013",
         ifelse(x == 0, "0",
                ifelse(x >= 1e-3, sub("\\.", ",", sprintf("%.4f", x)),
                       sub("e-0?", "\u00d710\u207b", sub("\\.", ",",
                                                         formatC(x, format = "e", digits = 2))))))
}

fmt_pct <- function(x) {
  ifelse(is.na(x), "\u2013",
         ifelse(x < 0.01, "<0,01", sub("\\.", ",", sprintf("%.2f", x))))
}

tez_tab <- tab %>%
  filter(R0 >= 0.5) %>%                      # 0,30 satiri gereksiz
  transmute(
    `R0`                  = sub("\\.", ",", sprintf("%.2f", R0)),
    `Rejim`               = recode(rejim, superkritik = "s\u00fcperkritik"),
    `P_est(tau=10)`       = fmt(tau_10),
    `P_est(tau=20)`       = fmt(tau_20),
    `P_est(tau=30)`       = fmt(tau_30),
    `P_est(sonsuz)`       = fmt(P_limit),
    `Fark (%)`            = fmt_pct(fark_pct)
  )

write_csv(tez_tab, file.path(out_tbl, "tbl_tau_sensitivity.csv"))
if (nrow(paylar) > 0)
  write_csv(bind_rows(
    paylar %>% mutate(duzey = "senaryo"),
    ilce_paylar %>% transmute(ssp, rejim = district, n = n_aktif,
                              oran_pct = superkritik_pct, duzey = "ilce")),
    file.path(out_tbl, "tbl_tau_regime_shares.csv"))

# --- Figur ---
fig_d <- expand_grid(R0 = seq(0.3, 2.5, by = 0.01),
                     tau = c(10L, 30L, 100L)) %>%
  mutate(P_est = p_est_analytic(R0, tau),
         seri = paste0("tau = ", tau))
fig_l <- tibble(R0 = seq(0.3, 2.5, by = 0.01)) %>%
  mutate(P_est = p_est_limit(R0),
         P_est = ifelse(R0 <= 1, NA_real_, P_est),   # subkritikte cizme
         seri = "tau -> sonsuz (klasik)")

p <- bind_rows(fig_d, fig_l) %>%
  mutate(P_plot = pmax(P_est, 1e-16)) %>%
  ggplot(aes(R0, P_plot, colour = seri, linetype = seri)) +
  geom_line(linewidth = 0.7) +
  geom_vline(xintercept = 1, linetype = "dotted", linewidth = 0.4) +
  annotate("text", x = 0.92, y = 1e-3, label = "subkritik", hjust = 1, size = 3) +
  annotate("text", x = 1.08, y = 1e-3, label = "superkritik", hjust = 0, size = 3) +
  scale_y_log10(labels = function(x) formatC(x, format = "e", digits = 0)) +
  scale_colour_manual(values = c("tau = 10" = "#2c7bb6", "tau = 30" = "#d7191c",
                                 "tau = 100" = "#fdae61",
                                 "tau -> sonsuz (klasik)" = "grey20")) +
  scale_linetype_manual(values = c("tau = 10" = "solid", "tau = 30" = "solid",
                                   "tau = 100" = "solid",
                                   "tau -> sonsuz (klasik)" = "dashed")) +
  labs(x = expression(R[0]), y = expression(P[est]~"(log olcek)"),
       colour = NULL, linetype = NULL,
       title = "Sonlu eşik seçiminin yerleşme olasılığına etkisi",
       subtitle = "Subkritik rejimde klasik ölçut sıfır verir; sonlu eşik zorunludur") +
  theme_bw(base_size = 10) + theme(legend.position = "bottom")

ggsave(file.path(out_fig, "fig_tau_sensitivity.png"), p,
       width = 8, height = 5, dpi = 300)

# =============================================================================
# 6) RAPOR
# =============================================================================
cat("=== TABLO 5.4.2.1 — tau duyarliligi (teze) ===\n")
as.data.frame(tez_tab) %>% print(row.names = FALSE)

cat("\n=== SAYISAL DOGRULAMA (analitik vs birinci-adim analizi, tau=30) ===\n")
dog %>% mutate(across(where(is.numeric), ~ signif(.x, 6))) %>%
  as.data.frame() %>% print(row.names = FALSE)
cat("Maks mutlak fark:", format(max(dog$mutlak_fark), scientific = TRUE, digits = 3), "\n")

if (nrow(paylar) > 0) {
  cat("\n=== AMPIRIK DESTEK: aktif hucrelerde rejim dagilimi ===\n")
  paylar %>% as.data.frame() %>% print(row.names = FALSE)

  cat("\n--- Ilce bazinda superkritik (R0>=1) hucre orani ve maks R0 ---\n")
  ilce_paylar %>% mutate(maks_R0 = round(maks_R0, 3)) %>%
    as.data.frame() %>% print(row.names = FALSE)

  sub_pct <- paylar %>% filter(grepl("^subkritik", rejim)) %>%
    summarise(o = round(mean(oran_pct), 1)) %>% pull(o)
  cat(sprintf(
"\n>>> Aktif ilce-ay hucrelerinin ortalama %%%.1f'i SUBKRITIKTIR (R0 < 1).\n", sub_pct))
  cat("    Bu hucrelerde klasik olcut P_est(sonsuz) = 0 verirdi; raporlanan\n",
      "    tum degerler sonlu esikten kaynaklanmaktadir.\n", sep = "")
}

cat("\nYazildi:\n  ", file.path(out_tbl, "tbl_tau_sensitivity.csv"),
    "\n  ", file.path(out_tbl, "tbl_tau_regime_shares.csv"),
    "\n  ", file.path(out_fig, "fig_tau_sensitivity.png"), "\n", sep = "")
