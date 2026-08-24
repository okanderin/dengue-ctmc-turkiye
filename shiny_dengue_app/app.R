# ==========================================================
# Dengue Yerleşme Riski — CTMC Spark-Phase Model
# Shiny Uygulaması (shinyapps.io uyumlu)
#
# Veri kaynağı: r_project_tez/outputs/_canonical (dondurulmuş kanonik
# çıktılar) + r_project_tez/outputs/tables (kanonik dondurmadan SONRA
# üretilen ek analizler, örn. tau-eşiği duyarlılığı) içinden bu
# uygulamanın outputs/ klasörüne senkronize edilir. R tarafındaki
# analiz pipeline'ı R/04_results altında core / validation / sensitivity /
# regression / diagnostics / reports olarak alt klasörlere ayrılmıştır
# (bkz. R/00_maintenance/14_restructure_04_results.R,
# 15_reorganize_04_results_faz2.R); bu uygulama yalnızca dondurulmuş
# CSV çıktılarını okur, pipeline script'lerine bağımlı değildir.
#
# Sekmeler:
#   1) What-If Hesaplayıcı     — anlık P_est / R0 hesabı (veri gerekmez)
#   2) Serbest Konum (P_ufuk)  — keyfi konum için 50 yıl kümülatif risk
#   3) Projeksiyon Tarayıcı    — pre-computed MC çıktıları (opsiyonel)
#   4) Duyarlılık & Doğrulama  — OAT/k_vpd/tau duyarlılığı + CTMC doğrulama
#   5) Hakkında                — model özeti, kullanım kılavuzu, referanslar
#
# Okan Derin — İstanbul Medipol Üniversitesi, 2026
# ==========================================================

# --------------------------------------------------------------
# PAKETLER
# --------------------------------------------------------------
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(DT)
library(scales)
library(gridExtra)

# readr opsiyonel — sadece Projeksiyon sekmesi için
has_readr <- requireNamespace("readr", quietly = TRUE)

# ==============================================================
# MODEL FONKSİYONLARI
# ==============================================================

briere <- function(T, c, T0, Tm) {
  out <- numeric(length(T))
  ok  <- is.finite(T) & T > T0 & T < Tm
  out[ok] <- c * T[ok] * (T[ok] - T0) * sqrt(Tm - T[ok])
  pmax(out, 0)
}

quadratic_unimodal <- function(T, c, T0, Tm) {
  out <- numeric(length(T))
  ok  <- is.finite(T) & T > T0 & T < Tm
  out[ok] <- -c * (T[ok] - T0) * (T[ok] - Tm)
  pmax(out, 0)
}

make_trait_table <- function(species = c("albopictus", "aegypti")) {
  species <- match.arg(species)
  if (species == "aegypti") {
    data.frame(
      trait = c("a_biting","eip_dev_rate","lifespan_temp","vpd_ref","k_vpd"),
      form  = c("briere","briere","quadratic","scalar","scalar"),
      c     = c(2.71e-4, 1.04e-4, 1.48e-1, 1.0, 0.5),
      T0    = c(14.67,   11.50,   9.16,  NA, NA),
      Tm    = c(41.00,   38.97,  37.73,  NA, NA),
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(
      trait = c("a_biting","eip_dev_rate","lifespan_temp","vpd_ref","k_vpd"),
      form  = c("briere","briere","quadratic","scalar","scalar"),
      c     = c(1.93e-4, 1.09e-4, 1.43e-1, 1.0, 0.5),
      T0    = c(10.25,   10.39,   6.24,  NA, NA),
      Tm    = c(38.32,   43.05,  38.25,  NA, NA),
      stringsAsFactors = FALSE
    )
  }
}

.get_row <- function(tab, trait_name) tab[tab$trait == trait_name, , drop = FALSE]

a_of_T <- function(T, tab) {
  r <- .get_row(tab, "a_biting")
  briere(T, r$c, r$T0, r$Tm)
}

eip_mean_of_T <- function(T, tab) {
  r    <- .get_row(tab, "eip_dev_rate")
  rate <- briere(T, r$c, r$T0, r$Tm)
  ifelse(rate > 0, 1 / rate, Inf)
}

vpd_kpa <- function(T, RH) {
  es <- 0.6108 * exp(17.27 * T / (T + 237.3))
  pmax(es * (1 - RH / 100), 0)
}

mu_v_of_TRH <- function(T, RH, tab, lf_floor = 0.25) {
  rT  <- .get_row(tab, "lifespan_temp")
  lfT <- pmax(quadratic_unimodal(T, rT$c, rT$T0, rT$Tm), lf_floor)
  muT <- 1 / lfT
  rk  <- .get_row(tab, "k_vpd")
  rv  <- .get_row(tab, "vpd_ref")
  pmax(muT * exp(rk$c * (vpd_kpa(T, RH) - rv$c)), 1e-6)
}

compute_R0 <- function(T, RH, m, beta_vh, beta_hv, gamma_day, tab) {
  aT  <- a_of_T(T, tab)
  muv <- mu_v_of_TRH(T, RH, tab)
  eip <- eip_mean_of_T(T, tab)
  if (!is.finite(eip) || eip <= 0 || aT <= 0 || muv <= 0) return(0)
  surv <- exp(-muv * eip)
  max((m * aT^2 * beta_vh * beta_hv * surv) / (muv * gamma_day), 0)
}

P_est_finite <- function(R0, tau = 30L) {
  if (R0 <= 0) return(0)
  rho <- 1 / R0
  if (abs(rho - 1) < 1e-10) return(1 / tau)
  (1 - rho) / (1 - rho^tau)
}

# ==============================================================
# SABİTLER
# ==============================================================

DISTRICT_LABELS <- c(
  "TUR.10.4_1"  = "Hopa (Artvin)",
  "TUR.39.3_1"  = "Eğirdir (Isparta)",
  "TUR.40.25_1" = "Kartal (İstanbul)",
  "TUR.59.4_1"  = "Fethiye (Muğla)",
  "TUR.81.6_1"  = "Zonguldak"
)

DISTRICT_ORDER <- c(
  "TUR.40.25_1","TUR.59.4_1","TUR.81.6_1",
  "TUR.10.4_1","TUR.39.3_1"
)

COL_DISTRICT <- c(
  "TUR.40.25_1" = "#E63946",
  "TUR.59.4_1"  = "#457B9D",
  "TUR.81.6_1"  = "#2A9D8F",
  "TUR.10.4_1"  = "#E9C46A",
  "TUR.39.3_1"  = "#264653"
)

SSP_LABELS <- c(ssp126 = "SSP1-2.6", ssp245 = "SSP2-4.5", ssp585 = "SSP5-8.5")
COL_SSP    <- c(ssp126 = "#2A9D8F",  ssp245 = "#E9C46A",   ssp585 = "#E63946")

AY_TR <- c("Oca","Şub","Mar","Nis","May","Haz",
           "Tem","Ağu","Eyl","Eki","Kas","Ara")

# Duyarlılık tablolarında (tbl_oat_*, tbl_kvpd_*) ilçe adları Türkçe
# aksansız (ASCII) yazılır — DISTRICT_LABELS ile eşleştirme için:
OAT_DISTRICT_ASCII <- c(
  "TUR.10.4_1"  = "Hopa (Artvin)",
  "TUR.39.3_1"  = "Egirdir (Isparta)",
  "TUR.40.25_1" = "Kartal (Istanbul)",
  "TUR.59.4_1"  = "Fethiye (Mugla)",
  "TUR.81.6_1"  = "Zonguldak"
)
KVPD_DISTRICT_ASCII <- c(
  "TUR.10.4_1"  = "Hopa",
  "TUR.39.3_1"  = "Egirdir",
  "TUR.40.25_1" = "Kartal",
  "TUR.59.4_1"  = "Fethiye",
  "TUR.81.6_1"  = "Zonguldak"
)
KVPD_ASCII_TO_ID <- setNames(names(KVPD_DISTRICT_ASCII), KVPD_DISTRICT_ASCII)

# ==============================================================
# YARDIMCI: pre-computed veri yükle (shinyapps.io: outputs/ klasörü)
# ==============================================================

load_precomputed <- function() {
  # shinyapps.io'da app.R ile aynı dizinde "outputs/" klasörü olmalı
  base_dir <- "outputs"
  if (!dir.exists(base_dir)) return(NULL)
  
  ssps <- c("ssp126", "ssp245", "ssp585")
  monthly_list <- list()
  yearly_list  <- list()
  
  for (ssp in ssps) {
    sim_dir <- file.path(base_dir, ssp, "simulation")
    if (!dir.exists(sim_dir)) next
    
    mf <- list.files(sim_dir, "ctmc_spark_monthly.*rep.*\\.csv$", full.names = TRUE)
    yf <- list.files(sim_dir, "ctmc_spark_yearly.*rep.*\\.csv$",  full.names = TRUE)
    
    read_csv_safe <- function(path) {
      if (!has_readr) return(tryCatch(read.csv(path), error = function(e) NULL))
      tryCatch(readr::read_csv(path, show_col_types = FALSE), error = function(e) NULL)
    }
    
    if (length(mf) > 0) {
      d <- read_csv_safe(sort(mf, decreasing = TRUE)[1])
      if (!is.null(d)) monthly_list[[ssp]] <- mutate(d, ssp = ssp)
    }
    if (length(yf) > 0) {
      d <- read_csv_safe(sort(yf, decreasing = TRUE)[1])
      if (!is.null(d)) yearly_list[[ssp]] <- mutate(d, ssp = ssp)
    }
  }
  
  monthly <- bind_rows(monthly_list)
  yearly  <- bind_rows(yearly_list)
  if (nrow(monthly) == 0 || nrow(yearly) == 0) return(NULL)
  
  monthly <- monthly %>%
    mutate(
      district_id    = factor(district_id, levels = DISTRICT_ORDER),
      district_label = DISTRICT_LABELS[as.character(district_id)],
      month_name     = factor(AY_TR[month], levels = AY_TR)
    )
  yearly <- yearly %>%
    mutate(
      district_id    = factor(district_id, levels = DISTRICT_ORDER),
      district_label = DISTRICT_LABELS[as.character(district_id)]
    )
  
  list(monthly = monthly, yearly = yearly)
}

# ==============================================================
# YARDIMCI: duyarlılık & doğrulama tabloları (opsiyonel)
# ==============================================================
# Bu tablolar r_project_tez/outputs/_canonical (dondurulmuş) ve
# r_project_tez/outputs/tables (dondurmadan sonra üretilen tau-eşiği
# duyarlılığı gibi ek analizler) klasörlerinden bu uygulamanın
# outputs/ dizinine kopyalanmıştır. Hiçbiri yoksa ilgili sekme
# bölümü sessizce gizlenir.

read_csv_safe0 <- function(path) {
  if (!file.exists(path)) return(NULL)
  if (!has_readr) return(tryCatch(read.csv(path, stringsAsFactors = FALSE), error = function(e) NULL))
  tryCatch(readr::read_csv(path, show_col_types = FALSE), error = function(e) NULL)
}

load_sensitivity <- function() {
  base_dir <- "outputs"
  if (!dir.exists(base_dir)) return(NULL)
  ssps <- c("ssp126", "ssp245", "ssp585")

  oat  <- read_csv_safe0(file.path(base_dir, "tables", "tbl_oat_tornado_pp.csv"))
  kvpd <- read_csv_safe0(file.path(base_dir, "tables", "tbl_kvpd_sensitivity.csv"))
  tau  <- read_csv_safe0(file.path(base_dir, "tables", "tbl_tau_sensitivity.csv"))
  tau_regime <- read_csv_safe0(file.path(base_dir, "tables", "tbl_tau_regime_shares.csv"))

  val_list <- list()
  for (ssp in ssps) {
    s1 <- read_csv_safe0(file.path(base_dir, ssp, "validation", "validation_stage1_summary.csv"))
    s2 <- read_csv_safe0(file.path(base_dir, ssp, "validation", "validation_stage2_jensen.csv"))
    if (!is.null(s1) && !is.null(s2)) {
      val_list[[ssp]] <- data.frame(
        Senaryo            = SSP_LABELS[ssp],
        "n_kombinasyon"    = s1$n_total,
        "n_aktif (lambda>0)" = s1$n_active,
        "Analitik uyum (%)" = s1$pass_pct,
        "Maks fark (Aşama 1)" = signif(s1$max_abs_diff, 3),
        "Jensen sapma, ort. (%)" = signif(s2$mean_pct_jensen, 3),
        "Jensen sapma, medyan (%)" = signif(s2$median_pct_jensen, 3),
        "n_aktif (Aşama 2)" = s2$n_active,
        check.names = FALSE, stringsAsFactors = FALSE
      )
    }
  }
  validation <- if (length(val_list) > 0) bind_rows(val_list) else NULL

  if (is.null(oat) && is.null(kvpd) && is.null(tau) && is.null(validation)) return(NULL)
  list(oat = oat, kvpd = kvpd, tau = tau, tau_regime = tau_regime, validation = validation)
}

# ==============================================================
# TEMA
# ==============================================================

theme_app <- function(base_size = 12) {
  theme_bw(base_size = base_size) +
    theme(
      panel.grid.minor  = element_blank(),
      panel.grid.major  = element_line(colour = "grey90", linewidth = 0.3),
      strip.background  = element_rect(fill = "grey95", colour = "grey70"),
      strip.text        = element_text(face = "bold"),
      legend.position   = "bottom",
      plot.title        = element_text(face = "bold"),
      plot.subtitle     = element_text(colour = "grey40")
    )
}

# ==============================================================
# CSS
# ==============================================================

app_css <- "
  .navbar-default { background-color: #264653 !important; border-color: #1d3640 !important; }
  .navbar-default .navbar-brand { color: #f1faee !important; font-weight: bold; }
  .navbar-default .navbar-nav > li > a { color: #a8dadc !important; }
  .navbar-default .navbar-nav > .active > a,
  .navbar-default .navbar-nav > .active > a:hover {
    background-color: #2a9d8f !important; color: white !important; }
  body { font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif; }
  .well { background-color: #f8f9fa; border: 1px solid #dee2e6; }
  .info-box { background:#e8f4f8; border-left:4px solid #2a9d8f;
              padding:12px 15px; margin-bottom:15px; border-radius:4px; }
  .warn-box { background:#fff3cd; border-left:4px solid #ffc107;
              padding:12px 15px; margin-bottom:15px; border-radius:4px; }
  .result-card { background:white; border:1px solid #dee2e6; border-radius:8px;
                 padding:20px; margin:10px 0; box-shadow:0 2px 4px rgba(0,0,0,.1); }
  .metric-big   { font-size:2.4em; font-weight:bold; color:#264653; }
  .metric-label { font-size:0.88em; color:#6c757d; margin-top:4px; }
  h4 { color:#264653; }
  .btn-teal { background-color:#2a9d8f !important; border-color:#2a9d8f !important;
              color:white !important; }
"

# ==============================================================
# UI
# ==============================================================

ui <- navbarPage(
  title = "Dengue Yerleşme Riski — CTMC Spark",
  header = tags$head(tags$style(HTML(app_css))),
  
  # ============================================================
  # SEKME 1: What-If Hesaplayıcı
  # ============================================================
  tabPanel("What-If Hesaplayıcı",
           fluidRow(column(12,
                           div(class = "info-box",
                               HTML("Kendi iklim ve model parametrelerinizi girerek tek ay için
             <strong>otoktona salgın yerleşme olasılığını (P<sub>est</sub>)</strong>
             ve <strong>R<sub>0</sub></strong>'ı hesaplayın.
             Deterministik EIP + sonlu-eşikli gambler's ruin (τ = 30) formülü kullanılır."))
           )),
           
           sidebarLayout(
             sidebarPanel(width = 4,
                          h4("İklim"),
                          sliderInput("wf_temp", "Sıcaklık (°C)",    5,  42, 25, 0.5),
                          sliderInput("wf_rh",   "Bağıl Nem (%)",   20, 100, 65, 1),
                          hr(),
                          h4("Model Parametreleri"),
                          selectInput("wf_species", "Sivrisinek Türü",
                                      choices  = c("Ae. albopictus" = "albopictus",
                                                   "Ae. aegypti"    = "aegypti"),
                                      selected = "albopictus"),
                          sliderInput("wf_m",       "Sivrisinek/İnsan (m)", 0.1, 5.0, 1.0, 0.1),
                          sliderInput("wf_beta_vh", "β_vh (vektör→insan)",  0.05, 0.80, 0.30, 0.01),
                          sliderInput("wf_beta_hv", "β_hv (insan→vektör)",  0.05, 0.80, 0.33, 0.01),
                          sliderInput("wf_ip",      "Enfeksiyöz dönem (gün)", 2, 14, 5, 1),
                          numericInput("wf_tau",    "Salgın eşiği (τ)", 30, 5, 100, 5),
                          hr(),
                          h4("İthalat Baskısı"),
                          numericInput("wf_lambda", "Aylık beklenen ithal vaka (λ)",
                                       0.10, min = 0, max = 100, step = 0.01),
                          hr(),
                          actionButton("wf_calc",  "Hesapla",
                                       class = "btn-primary btn-lg btn-block btn-teal"),
                          br(),
                          actionButton("wf_sweep", "Sıcaklık Taraması (5–42 °C)",
                                       class = "btn-info btn-block")
             ),
             
             mainPanel(width = 8,
                       # Metrik kartlar
                       fluidRow(
                         column(4, div(class = "result-card",
                                       div(class = "metric-label", "Temel Üreme Katsayısı"),
                                       div(class = "metric-big",   textOutput("wf_R0",    inline = TRUE)),
                                       div(class = "metric-label", "R₀")
                         )),
                         column(4, div(class = "result-card",
                                       div(class = "metric-label", "Yerleşme Olasılığı"),
                                       div(class = "metric-big",   textOutput("wf_Pest",  inline = TRUE)),
                                       div(class = "metric-label", "P_est (τ-sonlu)")
                         )),
                         column(4, div(class = "result-card",
                                       div(class = "metric-label", "Aylık Salgın Olas."),
                                       div(class = "metric-big",   textOutput("wf_Pmon",  inline = TRUE)),
                                       div(class = "metric-label", "q_import × P_est")
                         ))
                       ),
                       hr(),
                       fluidRow(
                         column(6, div(class = "result-card",
                                       h4("Ara Hesaplamalar"),
                                       tableOutput("wf_details")
                         )),
                         column(6, div(class = "result-card",
                                       h4("Kümülatif Risk Hesaplayıcı"),
                                       div(class = "info-box", style = "font-size:.85em;",
                                           HTML("Birden fazla ayın p_month değerlerini virgülle girin:
                   <em>P_kümülatif = 1 − ∏(1 − p<sub>ay</sub>)</em>")),
                                       textAreaInput("wf_multi", NULL,
                                                     placeholder = "örn: 0.001, 0.005, 0.01, 0.003",
                                                     rows = 2),
                                       actionButton("wf_cumul", "Hesapla",
                                                    class = "btn-outline-secondary btn-sm"),
                                       br(), br(),
                                       uiOutput("wf_cumul_out")
                         ))
                       ),
                       hr(),
                       plotOutput("wf_sweep_plot", height = "420px")
             )
           )
  ),
  
  # ============================================================
  # SEKME 2: Serbest Konum (P_ufuk)
  # ============================================================
  tabPanel("Serbest Konum (P_ufuk)",
           fluidRow(column(12,
                           div(class = "info-box",
                               HTML("Herhangi bir konum i&ccedil;in 12 ayl&#305;k iklim d&ouml;ng&uuml;s&uuml; ve ithalat bask&#305;s&#305; girerek
             y&#305;ll&#305;k ve <strong>50-y&#305;l k&uuml;m&uuml;latif salg&#305;n riski (P<sub>ufuk</sub>)</strong> hesaplay&#305;n.
             <em>P<sub>ufuk</sub> = 1 &minus; &prod;(1 &minus; p<sub>ay</sub>)</em>,
             p<sub>ay</sub> = (1 &minus; e<sup>&minus;&lambda;</sup>) &middot; P<sub>est</sub>. Model What-If ile ayn&#305;d&#305;r; veritaban&#305; gerekmez."))
           )),
           sidebarLayout(
             sidebarPanel(width = 4,
                          h4("Konum & Model"),
                          textInput("cl_name", "Konum ad\u0131", "\u00d6rnek Konum"),
                          selectInput("cl_species", "Sivrisinek T\u00fcr\u00fc",
                                      choices  = c("Ae. albopictus" = "albopictus",
                                                   "Ae. aegypti"    = "aegypti"),
                                      selected = "albopictus"),
                          sliderInput("cl_m",       "Sivrisinek/\u0130nsan (m)", 0.1, 5.0, 1.0, 0.1),
                          sliderInput("cl_beta_vh", "\u03b2_vh (vekt\u00f6r\u2192insan)", 0.05, 0.80, 0.30, 0.01),
                          sliderInput("cl_beta_hv", "\u03b2_hv (insan\u2192vekt\u00f6r)", 0.05, 0.80, 0.33, 0.01),
                          sliderInput("cl_ip",      "Enfeksiy\u00f6z d\u00f6nem (g\u00fcn)", 2, 14, 5, 1),
                          numericInput("cl_tau",    "Salg\u0131n e\u015fi\u011fi (\u03c4)", 30, 5, 100, 5),
                          hr(),
                          h4("Ayl\u0131k \u0130klim (Oca\u2192Ara, 12 de\u011fer)"),
                          textAreaInput("cl_temp", "S\u0131cakl\u0131k (\u00b0C)",
                                        "6, 7, 10, 15, 20, 25, 28, 28, 23, 17, 11, 7", rows = 2),
                          textAreaInput("cl_rh", "Ba\u011f\u0131l Nem (%)",
                                        "75, 74, 72, 70, 68, 65, 63, 64, 68, 72, 74, 76", rows = 2),
                          hr(),
                          h4("\u0130thalat Bask\u0131s\u0131"),
                          radioButtons("cl_imp_mode", "Giri\u015f bi\u00e7imi",
                                       c("Y\u0131ll\u0131k toplam (mevsimsel da\u011f\u0131t)" = "annual",
                                         "Ayl\u0131k 12 de\u011fer" = "monthly"),
                                       selected = "annual"),
                          conditionalPanel(
                            "input.cl_imp_mode == 'annual'",
                            numericInput("cl_lambda_year",
                                         "Y\u0131ll\u0131k beklenen viremik ithal vaka (\u039b)",
                                         1.0, min = 0, max = 1e4, step = 0.1)),
                          conditionalPanel(
                            "input.cl_imp_mode == 'monthly'",
                            textAreaInput("cl_lambda_m", "Ayl\u0131k \u03bb (12 de\u011fer)",
                                          "0,0,0,0.05,0.1,0.2,0.3,0.3,0.15,0.05,0,0", rows = 2)),
                          hr(),
                          h4("Projeksiyon"),
                          numericInput("cl_years", "Ufuk (y\u0131l)", 50, 1, 100, 1),
                          sliderInput("cl_trend", "Is\u0131nma e\u011filimi (\u00b0C/10 y\u0131l)", 0, 1.5, 0.3, 0.1),
                          hr(),
                          actionButton("cl_calc", "P_ufuk Hesapla",
                                       class = "btn-primary btn-lg btn-block btn-teal")
             ),
             mainPanel(width = 8,
                       fluidRow(
                         column(4, div(class = "result-card",
                                       div(class = "metric-label", "Y\u0131ll\u0131k Risk (1. y\u0131l)"),
                                       div(class = "metric-big", textOutput("cl_pyear", inline = TRUE)),
                                       div(class = "metric-label", "1 \u2212 \u220f(1\u2212p_ay)"))),
                         column(4, div(class = "result-card",
                                       div(class = "metric-label", "Ufuk K\u00fcm\u00fclatif"),
                                       div(class = "metric-big", style = "color:#e63946;",
                                           textOutput("cl_phorizon", inline = TRUE)),
                                       div(class = "metric-label", "P_ufuk"))),
                         column(4, div(class = "result-card",
                                       div(class = "metric-label", "Aktif ay"),
                                       div(class = "metric-big", textOutput("cl_active", inline = TRUE)),
                                       div(class = "metric-label", "P_est > 0")))
                       ),
                       hr(),
                       fluidRow(
                         column(6, div(class = "result-card",
                                       h4("Ayl\u0131k Ayr\u0131nt\u0131"),
                                       tableOutput("cl_month_tbl"))),
                         column(6, div(class = "result-card",
                                       h4("K\u00fcm\u00fclatif Risk E\u011frisi"),
                                       plotOutput("cl_curve", height = "300px")))
                       ),
                       hr(),
                       plotOutput("cl_month_plot", height = "320px")
             )
           )
  ),
  
  # ============================================================
  # SEKME 3: Projeksiyon Tarayıcı
  # ============================================================
  tabPanel("Projeksiyon Tarayıcı",
           fluidRow(column(12,
                           div(class = "info-box",
                               HTML("<strong>Projeksiyon Tarayıcı:</strong> Pre-computed Monte Carlo
             simülasyon çıktılarını (1000 tekrar) interaktif inceleyin.
             Çalıştırmak için <code>outputs/{ssp}/simulation/</code> CSV dosyalarını
             uygulamayla aynı dizine koyun."))
           )),
           
           sidebarLayout(
             sidebarPanel(width = 3,
                          h4("Filtreler"),
                          selectInput("proj_ssp", "İklim Senaryosu",
                                      choices  = c("Tümü"="all","SSP1-2.6"="ssp126",
                                                   "SSP2-4.5"="ssp245","SSP5-8.5"="ssp585"),
                                      selected = "all"),
                          checkboxGroupInput("proj_dist", "Sentinel İlçeler",
                                             choices  = setNames(DISTRICT_ORDER,
                                                                 DISTRICT_LABELS[DISTRICT_ORDER]),
                                             selected = DISTRICT_ORDER),
                          sliderInput("proj_yr", "Yıl Aralığı",
                                      2025, 2075, c(2025,2075), step = 1, sep = ""),
                          selectInput("proj_type", "Grafik Türü",
                                      choices  = c("Yıllık Risk Trendi"     = "trend",
                                                   "Mevsimsel P_est"        = "seasonal",
                                                   "Aylık Isı Haritası"     = "heatmap",
                                                   "Dekadal Karşılaştırma"  = "decade"),
                                      selected = "trend"),
                          selectInput("proj_scale", "Y Ekseni",
                                      choices = c("Logaritmik"="log","Lineer"="linear"),
                                      selected = "log")
             ),
             mainPanel(width = 9,
                       uiOutput("proj_warn"),
                       plotOutput("proj_plot", height = "480px"),
                       hr(),
                       h4("Se\u00e7ili d\u00f6nem k\u00fcm\u00fclatif risk (P_ufuk)"),
                       div(class = "info-box", style = "font-size:.85em;",
                           HTML("P<sub>ufuk</sub> = 1 &minus; &prod;<sub>y&#305;l</sub>(1 &minus; p<sub>y&#305;l</sub>),
                 se&ccedil;ili y&#305;l aral&#305;&#287;&#305;, il&ccedil;e ve senaryo filtreleri &uuml;zerinden hesaplan&#305;r.
                 Slider tam aral&#305;kta (2025&ndash;2075) iken ger&ccedil;ek 50-y&#305;l P<sub>ufuk</sub> elde edilir.")),
                       tableOutput("proj_horizon"),
                       hr(),
                       DTOutput("proj_table")
             )
           )
  ),
  
  # ============================================================
  # SEKME 4: Duyarlılık & Doğrulama
  # ============================================================
  tabPanel("Duyarlılık & Doğrulama",
           fluidRow(column(12,
                           div(class = "info-box",
                               HTML("Model çıktılarının <strong>doğrulaması</strong> (analitik çözümle
             karşılaştırma, EIP heterojenliği/Jensen sapması) ve parametre
             <strong>duyarlılık analizleri</strong> (tek-seferde-bir OAT, VPD-mortalite
             katsayısı k<sub>vpd</sub>, salgın eşiği τ). Kanonik çıktılardan
             (<code>outputs/_canonical</code>, <code>outputs/tables</code>) önceden hesaplanmıştır."))
           )),

           h4("1) CTMC Doğrulaması"),
           div(class = "result-card",
               p(style = "font-size:.85em; color:#555;",
                 "Aşama 1: Monte Carlo tekrarları (n_rep=1000), EIP sabitken (σ_EIP=0)
                 analitik sonlu-eşikli gambler's ruin çözümüyle makine hassasiyetinde
                 eşleşmelidir. Aşama 2: bireysel EIP ~ LogNormal iken (nested MC, iç n=2000)
                 gerçek E[P_est] ile deterministik P_est(E[EIP]) arasındaki fark
                 (Jensen sapması) ölçülür."),
               tableOutput("sens_validation")
           ),
           hr(),

           h4("2) Tek-Seferde-Bir (OAT) Duyarlılık"),
           div(class = "result-card",
               fluidRow(
                 column(4, selectInput("sens_oat_ssp", "İklim Senaryosu",
                                        choices  = c("SSP1-2.6"="ssp126","SSP2-4.5"="ssp245","SSP5-8.5"="ssp585"),
                                        selected = "ssp245")),
                 column(4, selectInput("sens_oat_dist", "İlçe",
                                        choices  = setNames(DISTRICT_ORDER, DISTRICT_LABELS[DISTRICT_ORDER]),
                                        selected = "TUR.40.25_1"))
               ),
               p(style = "font-size:.85em; color:#555;",
                 "m (sivrisinek/insan), β_vh/β_hv (bulaş olasılığı) ve ithalat baskısı (IP)
                 ±%20/%50/%100 değiştirildiğinde P_ufuk'un baz senaryoya göre değişimi
                 (yüzde puan, pp)."),
               plotOutput("sens_oat_plot", height = "340px")
           ),
           hr(),

           h4("3) VPD-Mortalite Katsayısı (k_vpd) Duyarlılığı"),
           div(class = "result-card",
               p(style = "font-size:.85em; color:#555;",
                 "μ_v = (1/lf(T))·exp(k_vpd·(VPD−VPD_ref)) formülündeki k_vpd referans
                 değeri (0.5 kPa⁻¹) yerine 0.3 ve 0.8 kPa⁻¹ ile P_ufuk yeniden hesaplanmıştır."),
               plotOutput("sens_kvpd_plot", height = "400px")
           ),
           hr(),

           h4("4) Salgın Eşiği (τ) Duyarlılığı"),
           div(class = "result-card",
               p(style = "font-size:.85em; color:#555;",
                 "Uygulamanın geri kalanında kullanılan τ=30 eşiğinin gerekçesi: farklı
                 R₀ rejimlerinde τ arttıkça P_est nasıl değişiyor?"),
               tableOutput("sens_tau_tbl")
           )
  ),

  # ============================================================
  # SEKME 5: Hakkında
  # ============================================================
  tabPanel("Hakkında",
           fluidRow(column(8, offset = 2,
                           div(class = "result-card",
                               h3("Dengue Yerleşme Riski — CTMC Spark Model"),
                               p("Bu uygulama, Türkiye'de dengue humması otoktona salgın yerleşme
          riskini değerlendiren CTMC spark-phase birth-death modelinin
          interaktif arayüzüdür."),
                               hr(),
                               h4("Sekmeler Nasıl Kullanılır"),
                               tags$ul(
                                 tags$li(HTML("<strong>What-If Hesaplayıcı</strong> — Tek bir sıcaklık/nem/tür/parametre
                       kombinasyonu için anlık R₀ ve P_est hesaplar; hiçbir veri dosyası
                       gerektirmez. Sıcaklığa göre R₀/P_est eğrisini görmek için
                       “Sıcaklık Taraması” düğmesini kullanın.")),
                                 tags$li(HTML("<strong>Serbest Konum (P_ufuk)</strong> — Sentinel ilçeler dışında,
                       kendi aylık sıcaklık/nem döngünüzü ve ithalat baskınızı girerek
                       herhangi bir konum için 1 yıllık ve N-yıllık (varsayılan 50)
                       kümülatif yerleşme riskini (P_ufuk) hesaplar; veri dosyası gerekmez.")),
                                 tags$li(HTML("<strong>Projeksiyon Tarayıcı</strong> — Beş sentinel ilçe için
                       önceden hesaplanmış 1000-tekrarlı Monte Carlo simülasyon çıktılarını
                       (2025–2075, 3 SSP senaryosu) yıl/senaryo/ilçe filtreleriyle inceler;
                       <code>outputs/{ssp}/simulation/</code> CSV dosyalarını gerektirir.")),
                                 tags$li(HTML("<strong>Duyarlılık &amp; Doğrulama</strong> — Modelin analitik çözümle
                       ne kadar örtüştüğünü (doğrulama), EIP heterojenliğinin etkisini
                       (Jensen sapması) ve temel parametrelerin (m, β, ithalat, k_vpd, τ)
                       sonuçları ne kadar değiştirdiğini (OAT/duyarlılık) gösterir.")),
                                 tags$li(HTML("<strong>Hakkında</strong> — bu sayfa: model özeti, sentinel ilçeler,
                       veri/kaynak bilgisi ve referanslar."))
                               ),
                               hr(),
                               h4("Model Özeti"),
                               tags$ul(
                                 tags$li(HTML("CTMC birth-death süreci; sonlu eşikli gambler's ruin (τ=30):
                       P<sub>est</sub> = (1−ρ) / (1−ρ<sup>τ</sup>)")),
                                 tags$li("Brière termal performans eğrileri (Mordecai et al. 2017)"),
                                 tags$li("Quadratic unimodal yaşam süresi + VPD nem düzeltmesi
                       (μ_v = 1/lf(T)·exp(k_vpd·(VPD−VPD_ref)))"),
                                 tags$li("Ross-Macdonald (indirgenmiş vektörel kapasite): R₀ = m·a²·β_vh·β_hv·exp(−μ_v·EIP) / (μ_v·γ)"),
                                 tags$li(HTML("<strong>EIP heterojenliği + Jensen düzeltmesi:</strong> sabit sıcaklıkta
                       bireysel EIP ~ LogNormal olarak modellenir; iç döngü (nested)
                       Monte Carlo (n=2000) ile E[P_est] tahmin edilir — bu, deterministik
                       EIP varsayımıyla P_est(E[EIP])'den sistematik olarak farklıdır
                       (bkz. “Duyarlılık &amp; Doğrulama” sekmesi, Aşama 2).")),
                                 tags$li("İthalat: GBD 2023 ülke-ağırlıklı, mevsimsellik ve viremi-anında
                       varış olasılığı ile ölçeklenen homojen olmayan Poisson süreci"),
                                 tags$li(HTML("Dış tekrar sayısı n_rep=1000, <em>tekrarlanabilirlik</em> içindir
                       (sabit tohum) — <em>parametrik belirsizlik değildir</em>; parametrik
                       belirsizlik ayrıca LHS–PRCC (küresel) ve OAT (tek-seferde-bir)
                       duyarlılık analizleriyle değerlendirilir.")),
                                 tags$li("Yağış tabanlı İklim Uygunluk İndeksi (CSI) ile mevsimsel
                       çapraz-doğrulama (Ae. aegypti/albopictus üreme uygunluğu)")
                               ),
                               hr(),
                               h4("Sentinel İlçeler"),
                               tags$ul(
                                 tags$li("Kartal (İstanbul) — Ae. albopictus"),
                                 tags$li("Fethiye (Muğla)   — Ae. albopictus"),
                                 tags$li("Zonguldak Merkez  — Ae. aegypti"),
                                 tags$li("Hopa (Artvin)     — Ae. aegypti"),
                                 tags$li("Eğirdir (Isparta) — Ae. albopictus")
                               ),
                               hr(),
                               h4("İklim Senaryoları"),
                               p("CMIP6 / CNRM-CM6-1-HR modeli; SSP1-2.6, SSP2-4.5, SSP5-8.5;
          aylık çözünürlük, 2015–2100."),
                               hr(),
                               h4("Veri ve Tekrarlanabilirlik"),
                               p(style = "font-size:.9em;",
                                 HTML("Bu uygulamadaki tüm önceden hesaplanmış çıktılar (Projeksiyon Tarayıcı,
          Duyarlılık &amp; Doğrulama), ana R analiz projesinin (<code>r_project_tez</code>)
          dondurulmuş kanonik çıktı klasöründen (<code>outputs/_canonical</code>) ve
          dondurmadan sonra üretilmiş ek duyarlılık tablolarından
          (<code>outputs/tables</code>) senkronize edilmiştir. Analiz pipeline'ı
          <code>R/01_setup → R/02_data → R/03_models → R/04_results</code> sırasıyla
          uçtan uca çalıştırılır (bkz. <code>R/run_all.R</code>); paket sürümleri
          <code>renv.lock</code> ile sabitlenmiştir.")),
                               hr(),
                               h4("Temel Referanslar"),
                               tags$ul(style = "font-size:.9em;",
                                       tags$li("Mordecai et al. (2017) PLOS NTD"),
                                       tags$li("Allen & Lahodny (2012) — CTMC salgın modelleri"),
                                       tags$li("Brand et al. (2021)    — Spark-phase yerleşme"),
                                       tags$li("Brière et al. (1999)   — Termal performans fonksiyonu")
                               ),
                               hr(),
                               p(style = "color:grey; font-size:.85em;",
                                 "Okan Derin — İstanbul Medipol Üniversitesi, Doktora Tezi, 2026", br(),
                                 "Danışman: Prof. Dr. Osman Erol Hayran")
                           )
           ))
  )
)

# ==============================================================
# SERVER
# ==============================================================

server <- function(input, output, session) {
  
  # ----------------------------------------------------------
  # Pre-computed veri (bir kez yükle)
  # ----------------------------------------------------------
  precomp <- reactive({ load_precomputed() })
  
  # ----------------------------------------------------------
  # WHAT-IF: hesap reaktifi
  # ----------------------------------------------------------
  wf <- eventReactive(input$wf_calc, {
    tab     <- make_trait_table(input$wf_species)
    T_c     <- input$wf_temp
    RH      <- input$wf_rh
    m       <- input$wf_m
    bvh     <- input$wf_beta_vh
    bhv     <- input$wf_beta_hv
    gamma   <- 1 / input$wf_ip
    tau     <- input$wf_tau
    lam     <- input$wf_lambda
    
    aT   <- a_of_T(T_c, tab)
    muv  <- mu_v_of_TRH(T_c, RH, tab)
    eip  <- eip_mean_of_T(T_c, tab)
    surv <- if (is.finite(eip) && eip > 0) exp(-muv * eip) else 0
    vpd  <- vpd_kpa(T_c, RH)
    R0   <- compute_R0(T_c, RH, m, bvh, bhv, gamma, tab)
    Pest <- P_est_finite(R0, tau)
    qimp <- 1 - exp(-lam)
    
    list(
      R0 = R0, Pest = Pest, pmon = qimp * Pest,
      qimp = qimp, aT = aT, muv = muv, eip = eip,
      surv = surv, vpd = vpd,
      lam_loc = if (muv > 0) (m * aT^2 * bvh * bhv * surv) / muv else 0,
      T_c = T_c, RH = RH, species = input$wf_species
    )
  }, ignoreNULL = FALSE)   # başlangıçta default değerlerle çalışsın
  
  fmt <- function(x) if (x < 1e-4) sprintf("%.3e", x) else sprintf("%.6f", x)
  
  output$wf_R0   <- renderText({ req(wf()); sprintf("%.4f", wf()$R0) })
  output$wf_Pest <- renderText({ req(wf()); fmt(wf()$Pest) })
  output$wf_Pmon <- renderText({ req(wf()); fmt(wf()$pmon) })
  
  output$wf_details <- renderTable({
    req(wf()); r <- wf()
    data.frame(
      Parametre = c("Tür","Sıcaklık (°C)","Bağıl Nem (%)","VPD (kPa)",
                    "a(T) ısırma hızı","μ_v mortalite","Yaşam süresi (gün)",
                    "EIP (gün)","EIP sağkalım","λ_local (i=1)",
                    "R₀","P_est","q_import","p_month"),
      Değer = c(
        ifelse(r$species == "aegypti","Ae. aegypti","Ae. albopictus"),
        sprintf("%.1f",  r$T_c),
        sprintf("%.0f",  r$RH),
        sprintf("%.3f",  r$vpd),
        sprintf("%.5f",  r$aT),
        sprintf("%.5f",  r$muv),
        sprintf("%.2f",  1/r$muv),
        if (is.finite(r$eip)) sprintf("%.2f",r$eip) else "∞ (sınır dışı)",
        sprintf("%.6f",  r$surv),
        sprintf("%.6f",  r$lam_loc),
        sprintf("%.6f",  r$R0),
        sprintf("%.3e",  r$Pest),
        sprintf("%.6f",  r$qimp),
        sprintf("%.3e",  r$pmon)
      ), stringsAsFactors = FALSE
    )
  }, striped = TRUE, hover = TRUE, width = "100%")
  
  # --- Kümülatif risk ---
  observeEvent(input$wf_cumul, {
    txt  <- trimws(input$wf_multi)
    vals <- suppressWarnings(
      as.numeric(unlist(strsplit(txt, "[,;[:space:]]+")))
    )
    vals <- vals[!is.na(vals)]
    if (length(vals) == 0) {
      output$wf_cumul_out <- renderUI(
        div(class="warn-box","Geçerli sayı girilmedi."))
      return()
    }
    vals <- pmin(pmax(vals, 0), 1)
    pc   <- 1 - prod(1 - vals)
    output$wf_cumul_out <- renderUI(div(
      div(class="metric-big", style="color:#e63946;", sprintf("%.4e", pc)),
      div(class="metric-label",
          sprintf("P_kümülatif — %d ay | değerler: %s",
                  length(vals),
                  paste(sprintf("%.2e",vals), collapse=", ")))
    ))
  })
  
  # --- Sıcaklık taraması ---
  observeEvent(input$wf_sweep, {
    tab   <- make_trait_table(input$wf_species)
    RH    <- input$wf_rh
    m     <- input$wf_m
    bvh   <- input$wf_beta_vh
    bhv   <- input$wf_beta_hv
    gamma <- 1 / input$wf_ip
    tau   <- input$wf_tau
    
    temps <- seq(5, 42, by = 0.5)
    sw <- data.frame(T = temps,
                     R0   = sapply(temps, \(t) compute_R0(t, RH, m, bvh, bhv, gamma, tab)),
                     Pest = 0)
    sw$Pest <- sapply(sw$R0, \(r) P_est_finite(r, tau))
    
    output$wf_sweep_plot <- renderPlot({
      p1 <- ggplot(sw, aes(T, R0)) +
        geom_line(linewidth = 1.2, colour = "#2a9d8f") +
        geom_hline(yintercept = 1, linetype = "dashed", colour = "#e63946") +
        annotate("text", x = 39, y = 1.05, label = "R₀ = 1",
                 colour = "#e63946", size = 3.5) +
        geom_vline(xintercept = input$wf_temp,
                   linetype = "dotted", colour = "grey40") +
        labs(x = "Sıcaklık (°C)", y = "R₀",
             title = paste("R₀ — ", ifelse(input$wf_species=="aegypti",
                                           "Ae. aegypti","Ae. albopictus")),
             subtitle = sprintf("m=%.1f  β_vh=%.2f  β_hv=%.2f  IP=%dd  RH=%d%%",
                                m, bvh, bhv, input$wf_ip, RH)) +
        theme_app()
      
      sw2 <- sw[sw$Pest > 0, ]
      p2 <- ggplot(sw2, aes(T, Pest)) +
        geom_line(linewidth = 1.2, colour = "#e63946") +
        geom_vline(xintercept = input$wf_temp,
                   linetype = "dotted", colour = "grey40") +
        scale_y_log10(labels = label_scientific()) +
        labs(x = "Sıcaklık (°C)", y = "P_est (log)",
             title = "Yerleşme Olasılığı vs Sıcaklık") +
        theme_app()
      
      grid.arrange(p1, p2, ncol = 2)
    })
  })
  
  # ----------------------------------------------------------
  # SERBEST KONUM / P_ufuk
  # ----------------------------------------------------------
  parse_vec <- function(txt, n = NULL) {
    v <- suppressWarnings(as.numeric(unlist(strsplit(trimws(txt), "[,;[:space:]]+"))))
    v <- v[!is.na(v)]
    if (!is.null(n) && length(v) > 0) {
      if (length(v) < n) v <- c(v, rep(tail(v, 1), n - length(v)))
      v <- v[seq_len(n)]
    }
    v
  }
  
  cl <- eventReactive(input$cl_calc, {
    tab   <- make_trait_table(input$cl_species)
    m     <- input$cl_m
    bvh   <- input$cl_beta_vh
    bhv   <- input$cl_beta_hv
    gamma <- 1 / input$cl_ip
    tau   <- input$cl_tau
    Tm    <- parse_vec(input$cl_temp, 12)
    RHm   <- parse_vec(input$cl_rh, 12)
    if (length(Tm) < 12 || length(RHm) < 12) return(NULL)
    
    if (input$cl_imp_mode == "monthly") {
      lam_m <- parse_vec(input$cl_lambda_m, 12)
      if (length(lam_m) < 12) lam_m <- rep(0, 12)
    } else {
      R0b   <- vapply(1:12, function(i) compute_R0(Tm[i], RHm[i], m, bvh, bhv, gamma, tab), numeric(1))
      Pestb <- vapply(R0b, function(r) P_est_finite(r, tau), numeric(1))
      w <- if (sum(Pestb) > 0) Pestb / sum(Pestb) else rep(1/12, 12)
      lam_m <- input$cl_lambda_year * w
    }
    
    years <- max(1L, as.integer(input$cl_years))
    trend <- input$cl_trend
    p_year  <- numeric(years)
    detail1 <- NULL
    for (y in seq_len(years)) {
      dT    <- trend * (y - 1) / 10
      R0m   <- vapply(1:12, function(i) compute_R0(Tm[i] + dT, RHm[i], m, bvh, bhv, gamma, tab), numeric(1))
      Pestm <- vapply(R0m, function(r) P_est_finite(r, tau), numeric(1))
      qimp  <- 1 - exp(-lam_m)
      pmon  <- qimp * Pestm
      p_year[y] <- 1 - prod(1 - pmon)
      if (y == 1)
        detail1 <- data.frame(month = AY_TR, T = Tm, RH = RHm,
                              lambda = lam_m, R0 = R0m, Pest = Pestm, pmon = pmon,
                              stringsAsFactors = FALSE)
    }
    cum_curve <- 1 - cumprod(1 - p_year)
    list(detail = detail1, p_year1 = p_year[1],
         P_horizon = tail(cum_curve, 1), cum = cum_curve,
         years = years, active = sum(detail1$Pest > 0), name = input$cl_name)
  }, ignoreNULL = TRUE)
  
  output$cl_pyear    <- renderText({ req(cl()); fmt(cl()$p_year1) })
  output$cl_phorizon <- renderText({ req(cl()); fmt(cl()$P_horizon) })
  output$cl_active   <- renderText({ req(cl()); sprintf("%d / 12", cl()$active) })
  
  output$cl_month_tbl <- renderTable({
    req(cl()); d <- cl()$detail
    data.frame(
      Ay     = d$month,
      T      = sprintf("%.1f", d$T),
      RH     = sprintf("%.0f", d$RH),
      lambda = sprintf("%.3f", d$lambda),
      R0     = sprintf("%.3f", d$R0),
      P_est  = sprintf("%.2e", d$Pest),
      p_ay   = sprintf("%.2e", d$pmon),
      check.names = FALSE, stringsAsFactors = FALSE
    )
  }, striped = TRUE, hover = TRUE, width = "100%")
  
  output$cl_month_plot <- renderPlot({
    req(cl()); d <- cl()$detail
    d$month <- factor(d$month, levels = AY_TR)
    d2 <- d[d$Pest > 0, ]
    validate(need(nrow(d2) > 0, "Hi\u00e7bir ayda P_est > 0 de\u011fil."))
    ggplot(d2, aes(month, Pest)) +
      geom_col(fill = "#2a9d8f") +
      scale_y_log10(labels = label_scientific()) +
      labs(x = NULL, y = "P_est (log)",
           title = paste("Ayl\u0131k yerle\u015fme olas\u0131l\u0131\u011f\u0131 \u2014", cl()$name)) +
      theme_app()
  })
  
  output$cl_curve <- renderPlot({
    req(cl())
    cc <- data.frame(year = seq_len(cl()$years), cum = cl()$cum)
    ggplot(cc, aes(year, cum)) +
      geom_line(linewidth = 1.2, colour = "#e63946") +
      labs(x = "Y\u0131l", y = "K\u00fcm\u00fclatif P",
           title = sprintf("K\u00fcm\u00fclatif salg\u0131n riski (%d y\u0131l)", cl()$years)) +
      theme_app()
  })
  
  # ----------------------------------------------------------
  # PROJEKSİYON TARAYICI
  # ----------------------------------------------------------
  output$proj_warn <- renderUI({
    if (is.null(precomp()))
      div(class = "warn-box",
          HTML("<strong>Veri bulunamadı.</strong>
               <code>outputs/{ssp}/simulation/</code> CSV dosyaları
               uygulamayla aynı dizinde olmalı.
               <em>What-If sekmesi veri gerektirmez.</em>"))
  })
  
  proj_y <- reactive({
    req(precomp())
    d <- precomp()$yearly
    if (input$proj_ssp != "all") d <- filter(d, ssp == input$proj_ssp)
    filter(d,
           district_id %in% input$proj_dist,
           year >= input$proj_yr[1],
           year <= input$proj_yr[2])
  })
  
  proj_m <- reactive({
    req(precomp())
    d <- precomp()$monthly
    if (input$proj_ssp != "all") d <- filter(d, ssp == input$proj_ssp)
    filter(d,
           district_id %in% input$proj_dist,
           year >= input$proj_yr[1],
           year <= input$proj_yr[2])
  })
  
  # --- P_ufuk: secili donem kumulatif risk tablosu ---
  output$proj_horizon <- renderTable({
    req(precomp()); d <- proj_y(); req(nrow(d) > 0)
    cum_fun <- function(p) {
      p <- p[is.finite(p)]
      if (!length(p)) return(NA_real_)
      1 - prod(1 - pmin(pmax(p, 0), 1))
    }
    fmt_p <- function(x) ifelse(!is.finite(x), "\u2014",
                                ifelse(x < 1e-4, formatC(x, format = "e", digits = 2),
                                       formatC(x, format = "f", digits = 4)))
    tab <- d %>%
      group_by(ssp, district_id, district_label) %>%
      summarise(
        n_year = dplyr::n(),
        Pm  = cum_fun(p_ge1_major_year_mean),
        Plo = cum_fun(p_ge1_major_year_p2_5),
        Phi = cum_fun(p_ge1_major_year_p97_5),
        .groups = "drop"
      ) %>%
      arrange(desc(Pm))
    data.frame(
      Senaryo    = SSP_LABELS[as.character(tab$ssp)],
      "\u0130l\u00e7e" = tab$district_label,
      "Y\u0131l"  = tab$n_year,
      P_ufuk     = fmt_p(tab$Pm),
      "%95 GA"   = paste0(fmt_p(tab$Plo), " \u2013 ", fmt_p(tab$Phi)),
      check.names = FALSE, stringsAsFactors = FALSE
    )
  }, striped = TRUE, hover = TRUE, width = "100%")
  
  output$proj_plot <- renderPlot({
    req(precomp())
    type <- input$proj_type
    
    if (type == "trend") {
      d <- proj_y(); req(nrow(d) > 0)
      p <- ggplot(d, aes(year, p_ge1_major_year_mean, colour = district_id)) +
        geom_line(linewidth = 0.8) +
        geom_ribbon(aes(ymin = p_ge1_major_year_p2_5,
                        ymax = p_ge1_major_year_p97_5,
                        fill = district_id),
                    alpha = 0.12, colour = NA) +
        scale_colour_manual(values=COL_DISTRICT, labels=DISTRICT_LABELS, name="İlçe") +
        scale_fill_manual(values=COL_DISTRICT, labels=DISTRICT_LABELS, guide="none") +
        labs(x="Yıl", y="P[≥1 majör/yıl]",
             title="Yıllık Otoktona Salgın Riski") +
        theme_app()
      if (input$proj_ssp == "all")
        p <- p + facet_wrap(~ssp, labeller=labeller(ssp=SSP_LABELS))
      if (input$proj_scale == "log")
        p <- p + scale_y_log10(labels=label_scientific())
      p
      
    } else if (type == "seasonal") {
      d <- proj_m() %>%
        filter(p_establishment_mean > 0) %>%
        group_by(district_id, district_label, month_name, ssp) %>%
        summarise(pm = mean(p_establishment_mean, na.rm=TRUE), .groups="drop")
      req(nrow(d) > 0)
      p <- ggplot(d, aes(month_name, pm, group=district_label, colour=district_id)) +
        geom_line(linewidth=0.8) + geom_point(size=2) +
        scale_colour_manual(values=COL_DISTRICT,labels=DISTRICT_LABELS,name="İlçe") +
        labs(x=NULL, y="P_est", title="Mevsimsel Yerleşme Profili") +
        theme_app()
      if (input$proj_ssp == "all")
        p <- p + facet_wrap(~ssp, labeller=labeller(ssp=SSP_LABELS))
      if (input$proj_scale == "log")
        p <- p + scale_y_log10(labels=label_scientific())
      p
      
    } else if (type == "heatmap") {
      d <- proj_m()
      if (input$proj_ssp == "all") d <- filter(d, ssp=="ssp245")
      d <- d %>%
        group_by(district_label, month_name) %>%
        summarise(lam = mean(lambda_local_i1_mean, na.rm=TRUE), .groups="drop")
      req(nrow(d) > 0)
      ggplot(d, aes(month_name, district_label, fill=lam)) +
        geom_tile(colour="white", linewidth=0.4) +
        geom_text(aes(label=sprintf("%.2g",lam)), size=3) +
        scale_fill_gradient(low="lightyellow", high="firebrick",
                            trans="log10", name="λ_local") +
        labs(x=NULL, y=NULL, title="Aylık Yerel Bulaş Hızı") +
        theme_app() + theme(axis.text.x=element_text(angle=45,hjust=1))
      
    } else {  # decade
      d <- proj_y()
      if (input$proj_ssp == "all") d <- filter(d, ssp=="ssp245")
      d <- d %>%
        mutate(decade = paste0(floor(year/10)*10,"s")) %>%
        group_by(district_id, district_label, decade) %>%
        summarise(pm = mean(p_ge1_major_year_mean, na.rm=TRUE), .groups="drop")
      req(nrow(d) > 0)
      ggplot(d, aes(decade, pm, fill=district_id)) +
        geom_col(position=position_dodge(.8), width=0.7,
                 colour="grey30", linewidth=0.2) +
        scale_fill_manual(values=COL_DISTRICT,labels=DISTRICT_LABELS,name="İlçe") +
        scale_y_continuous(labels=label_scientific()) +
        labs(x="On Yıl", y="Ort. yıllık risk",
             title="Dekadal Risk Progresyonu") +
        theme_app()
    }
  })
  
  output$proj_table <- renderDT({
    req(precomp())
    proj_y() %>%
      group_by(İlçe = district_label, SSP = ssp) %>%
      summarise(
        `Ort. P_yıl` = sprintf("%.3e", mean(p_ge1_major_year_mean, na.rm=TRUE)),
        `Maks P_yıl` = sprintf("%.3e", max(p_ge1_major_year_mean,  na.rm=TRUE)),
        `Min P_yıl`  = sprintf("%.3e", min(p_ge1_major_year_mean,  na.rm=TRUE)),
        .groups = "drop"
      ) %>%
      datatable(options = list(pageLength=15, dom="t"), rownames=FALSE)
  })

  # ----------------------------------------------------------
  # DUYARLILIK & DOĞRULAMA
  # ----------------------------------------------------------
  sens <- reactive({ load_sensitivity() })

  output$sens_validation <- renderTable({
    v <- sens()$validation
    validate(need(!is.null(v), "Doğrulama tabloları bulunamadı (outputs/{ssp}/validation/)."))
    v
  }, striped = TRUE, hover = TRUE, width = "100%")

  output$sens_oat_plot <- renderPlot({
    d0 <- sens()$oat
    validate(need(!is.null(d0), "OAT duyarlılık tablosu bulunamadı (outputs/tables/tbl_oat_tornado_pp.csv)."))
    d <- d0 %>%
      filter(ssp == SSP_LABELS[[input$sens_oat_ssp]],
             district == OAT_DISTRICT_ASCII[[input$sens_oat_dist]])
    validate(need(nrow(d) > 0, "Seçili senaryo/ilçe için OAT verisi yok."))
    ggplot(d, aes(x = reorder(sinif, delta_pp), y = delta_pp, fill = delta_pp > 0)) +
      geom_col(width = 0.65) +
      geom_hline(yintercept = 0, colour = "grey40") +
      coord_flip() +
      scale_fill_manual(values = c("TRUE" = "#2a9d8f", "FALSE" = "#e63946"), guide = "none") +
      labs(x = NULL, y = "Δ P_ufuk (yüzde puan)",
           title = sprintf("OAT duyarlılığı — %s, %s",
                            DISTRICT_LABELS[[input$sens_oat_dist]],
                            SSP_LABELS[[input$sens_oat_ssp]])) +
      theme_app()
  })

  output$sens_kvpd_plot <- renderPlot({
    d0 <- sens()$kvpd
    validate(need(!is.null(d0), "k_vpd duyarlılık tablosu bulunamadı (outputs/tables/tbl_kvpd_sensitivity.csv)."))
    d <- d0 %>%
      filter(Senaryo == SSP_LABELS[[input$sens_oat_ssp]], P_horizon > 0) %>%
      mutate(district_id = factor(KVPD_ASCII_TO_ID[Ilce], levels = DISTRICT_ORDER))
    validate(need(nrow(d) > 0, "Seçili senaryo için k_vpd verisi yok."))
    ggplot(d, aes(factor(k_vpd), P_horizon, colour = district_id, group = district_id)) +
      geom_line(linewidth = 0.9) +
      geom_point(size = 2.4) +
      scale_colour_manual(values = COL_DISTRICT, labels = DISTRICT_LABELS, name = "İlçe") +
      scale_y_log10(labels = label_scientific()) +
      labs(x = "k_vpd (kPa⁻¹)", y = "P_ufuk (log)",
           title = paste("k_vpd duyarlılığı —", SSP_LABELS[[input$sens_oat_ssp]])) +
      theme_app()
  })

  output$sens_tau_tbl <- renderTable({
    t <- sens()$tau
    validate(need(!is.null(t), "τ eşiği duyarlılık tablosu bulunamadı (outputs/tables/tbl_tau_sensitivity.csv)."))
    t
  }, striped = TRUE, hover = TRUE, width = "100%")
}

# ==============================================================
# LAUNCH
# ==============================================================
shinyApp(ui, server)
