## =========================================================
## R/04_results/01_generate_ssp_outputs.R          (rev. 2026-08, v2)
## Her SSP senaryosu için grafik ve tablo üretimi
##
## Kullanım:
##   Sys.setenv(SSP_SCENARIO = "ssp245")
##   source("R/04_results/01_generate_ssp_outputs.R")
##
## Veya tüm SSP'ler için:
##   for (s in c("ssp126","ssp245","ssp585"))  {
##     Sys.setenv(SSP_SCENARIO = s)
##     source("R/04_results/01_generate_ssp_outputs.R")
##   }
##
## Çıktılar: outputs/{ssp}/figures/ ve outputs/{ssp}/tables/
##
## ---------------------------------------------------------
## v2'DE NE DEĞİŞTİ — çalıştırmadan önce okuyun
##
## [1] PROVENANS DENETİMİ EKLENDİ (Bölüm 0).
##     load_ssp_data() dosyayı file.mtime() ile seçiyor ve regex
##     ("ctmc_spark_monthly.*rep.*") Bernoulli dönemi çıktılarıyla da
##     eşleşiyor. Bulut senkronize klasörde mtime güvenilir değildir.
##     Ayrıca monthly/yearly/horizon BAĞIMSIZ seçildiğinden farklı
##     koşulardan gelebilirler. Artık yükleme sonrası dört zorunlu
##     doğrulama yapılıyor; başarısızlıkta betik durur.
##
## [2] LOG EKSENLİ GRAFİKLERDE SESSİZ VERİ KAYBI GİDERİLDİ.
##     scale_y_log10, sıfır ve negatif değerleri uyarı vermeden düşürür.
##     SSP5-8.5 / Hopa / 2028'de p_ge1_major_year_mean TAM OLARAK 0'dır
##     (pipeline'da 1-exp(-x) katastrofik iptali; x ~ 1,7e-18).
##     Ayrıca ~1,1e-16 altındaki bütün değerler makine epsilon'unun
##     katlarına kuantize olmuştur; sinyal değil sayısal gürültüdür.
##     NOISE_FLOOR sabiti eklendi, sansürlenen satır sayısı raporlanıyor.
##     KALICI ÇÖZÜM pipeline'dadır: 1-exp(-x) yerine -expm1(-x).
##
## [3] LHS-PRCC ÇIKTISI R0'DIR, P_ufuk DEĞİLDİR (Bölüm 10).
##     compute_R0_lhs() R0 hesaplar ve PRCC R0'a karşı alınır. Tez
##     metnindeki "P_ufuk" ifadeleri KODLA ÇELİŞMEKTEDİR. Bkz. Bölüm 10
##     başındaki uyarı bloğu.
##
## [4] PRCC GÜVEN ARALIĞI ARTIK GERÇEKTEN BOOTSTRAP.
##     v1'de cor.test()$conf.int (Fisher z) kullanılıyordu; tez metni
##     "bootstrap güven aralığı" diyor. Nokta kestirimleri değişmez.
##
## [5] BÖLÜM 15'TE T_opt ARTIK M2'DEN HESAPLANIYOR.
##     v1, Lambda_import içeren M3'ten T_opt türetiyordu; bu, tezde
##     geçersiz sayılan "OLS ~19 C" eşdoğrusallık artefaktını üretir.
##
## [6] BÖLÜM 15 ve 16 ÇIKTI ADLARI DEĞİŞTİRİLDİ.
##     Bu bölümler tezin 6.10 regresyon tablolarını ÜRETMEZ; farklı
##     bağımlı değişken kullanırlar. Kanonik kaynak 6_10_pest_regression.R.
##     Dosya adı çakışması bu nedenle kaldırıldı.
##
## [7] BÖLÜM 16 SESSİZ ÇALIŞIYORDU.
##     source() içinde üst düzey ifadeler otomatik yazdırılmaz; anova(),
##     tidy(), VarCorr() satırları hiçbir çıktı üretmiyordu. print()'e
##     alındı, sonuçlar diske yazılıyor ve DONE bloğunun ÜSTÜNE taşındı.
##
## [8] BÖLÜM 14 EKSEN ETİKETİ DÜZELTİLDİ.
##     Etiket "λ_local > 0" diyordu; hesap LAMBDA_THRESH = 1e-4 ile
##     yapılıyor (tez tanımıyla uyumlu olan budur).
##
## [9] Bölüm 6 "Model doğrulaması" -> "Aşama 2: tahmin edici
##     karşılaştırması" olarak yeniden adlandırıldı (Aşama 1, sigma_EIP=0
##     koşusu gerektirir ve bu betikte yapılmaz).
## =========================================================

source("R/04_results/00_results_setup.R")

SSP <- Sys.getenv("SSP_SCENARIO", unset = "ssp245")
SSP_LABEL <- SSP_LABELS[SSP]

cat("\n", strrep("=", 55), "\n")
cat("Generating outputs for:", SSP_LABEL, "\n")
cat(strrep("=", 55), "\n\n")

# ---- Dizinler ----
DIR_FIG   <- here("outputs", SSP, "figures")
DIR_TBL   <- here("outputs", SSP, "tables")
dir.create(DIR_FIG, recursive = TRUE, showWarnings = FALSE)
dir.create(DIR_TBL, recursive = TRUE, showWarnings = FALSE)

# ---- Veri yükle ----
dat <- load_ssp_data(SSP)
monthly <- dat$monthly
yearly  <- dat$yearly
horizon <- dat$horizon

gamma_val <- 1 / 5      # gun^-1  (ip = 5 gun; tez baz degeri)
TAU_VAL   <- 30L

## Sayisal gurultu tabani. Cift hassasiyette 1-exp(-x) ifadesi
## x < ~1e-16 icin tam olarak 0 dondurur; bu esigin altindaki butun
## degerler eps/2 = 1.11e-16 katlarina kuantize olmus artefaktlardir.
NOISE_FLOOR <- 1e-15

cat("Data loaded:\n")
cat("  Monthly rows:", nrow(monthly), "\n")
cat("  Year range:", range(monthly$year), "\n")
cat("  Districts:", nlevels(monthly$district_id), "\n\n")


## =========================================================
## 0) PROVENANS DOĞRULAMASI  (v2'de eklendi)
## ---------------------------------------------------------
## Amac: load_ssp_data()'nin mtime tabanli dosya secimi nedeniyle
## Bernoulli donemi (Poisson inceltmesi oncesi) cikti agacindan veri
## okunmasini ve monthly/yearly/horizon'un farkli kosulardan gelmesini
## engellemek.
##
## Dort denetim:
##   D1  Poisson inceltme ozdesligi:
##         p_ay,major == -expm1(-lambda_import * p_est)
##       Bernoulli donemi dosyalarda bu saglanmaz (sapma ~1e-1).
##   D2  yearly, monthly'den yeniden kurulabiliyor mu?
##   D3  horizon, yearly'den yeniden kurulabiliyor mu?
##   D4  Lambda toplamlari tutarli mi?
## =========================================================
cat(">>> 0) Provenans dogrulamasi <<<\n")

TOL_IDENT   <- 1e-5    # dis-MC Jensen artigi: gozlenen <= 1e-6
TOL_REBUILD <- 1e-5
TOL_LAMBDA  <- 1e-9

.fail <- function(...) stop("[PROVENANS] ", ..., call. = FALSE)

# --- D1: Poisson inceltme ozdesligi ---
dev_poisson <- with(monthly,
  max(abs(p_month_major_mean - (-expm1(-lambda_import * p_establishment_mean))),
      na.rm = TRUE))

# Karsilastirma icin Bernoulli kurgusunun sapmasi (tani amacli)
dev_bernoulli <- if ("q_import_month" %in% names(monthly)) {
  with(monthly, max(abs(p_month_major_mean - q_import_month * p_establishment_mean),
                    na.rm = TRUE))
} else NA_real_

cat(sprintf("  D1 Poisson ozdesligi     : sapma = %.3e (tolerans %.0e)\n",
            dev_poisson, TOL_IDENT))
if (!is.na(dev_bernoulli))
  cat(sprintf("     (karsilastirma) Bernoulli sapmasi = %.3e\n", dev_bernoulli))

if (dev_poisson > TOL_IDENT) {
  .fail("p_ay,major sutunu Poisson inceltmesiyle uretilmemis gorunuyor.\n",
        "  Muhtemel neden: outputs/", SSP, "/simulation/ icinde Bernoulli donemi\n",
        "  (ctmc_mc_rep1000) dosyasi var ve file.mtime() onu sectirdi.\n",
        "  Cozum: eski dosyalari simulation/ disina tasiyin veya\n",
        "  load_ssp_data()'yi dosya adiyla sabitleyin.")
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

cat(sprintf("  D2 yearly <- monthly     : sapma = %.3e\n", dev_yearly))
if (!is.finite(dev_yearly) || dev_yearly > TOL_REBUILD)
  .fail("yearly dosyasi monthly'den yeniden kurulamiyor.\n",
        "  monthly ve yearly farkli kosulardan gelmis olabilir.")

# --- D3: horizon <- yearly ---
hz_rebuilt <- yearly %>%
  group_by(district_id) %>%
  summarise(rec = 1 - prod(1 - p_ge1_major_year_mean), .groups = "drop")

dev_horizon <- horizon %>%
  select(district_id, p_ge1_major_mean) %>%
  inner_join(hz_rebuilt, by = "district_id") %>%
  summarise(d = max(abs(p_ge1_major_mean - rec), na.rm = TRUE)) %>%
  pull(d)

cat(sprintf("  D3 horizon <- yearly     : sapma = %.3e\n", dev_horizon))
if (!is.finite(dev_horizon) || dev_horizon > TOL_REBUILD)
  .fail("horizon dosyasi yearly'den yeniden kurulamiyor.")

# --- D4: Lambda tutarliligi ---
dev_lambda <- yearly %>%
  group_by(district_id) %>%
  summarise(lam = sum(Lambda_import_year), .groups = "drop") %>%
  inner_join(select(horizon, district_id, Lambda_import), by = "district_id") %>%
  summarise(d = max(abs(lam - Lambda_import), na.rm = TRUE)) %>%
  pull(d)

cat(sprintf("  D4 Lambda tutarliligi    : sapma = %.3e\n", dev_lambda))
if (!is.finite(dev_lambda) || dev_lambda > TOL_LAMBDA)
  .fail("Lambda_import toplamlari yearly ile horizon arasinda tutarsiz.")

# --- Ozet ve olcek tanisi ---
lam_kartal <- horizon %>%
  filter(district_id == "TUR.40.25_1") %>% pull(Lambda_import)
if (length(lam_kartal) == 1) {
  cat(sprintf("  Kartal Lambda_ufuk = %.1f\n", lam_kartal))
  if (lam_kartal < 100)
    warning("[PROVENANS] Kartal Lambda_ufuk < 100. Poisson sonrasi kanonik\n",
            "  deger 228-290 araligindadir. Ithalat baskisi 3 gunluk pencere\n",
            "  ile hesaplanmis eski bir agactan okumus olabilirsiniz.",
            call. = FALSE)
}

prov_tbl <- tibble(
  Denetim = c("D1 Poisson ozdesligi", "D2 yearly<-monthly",
              "D3 horizon<-yearly", "D4 Lambda tutarliligi",
              "Kartal Lambda_ufuk"),
  Sapma   = c(formatC(dev_poisson, format = "e", digits = 2),
              formatC(dev_yearly,  format = "e", digits = 2),
              formatC(dev_horizon, format = "e", digits = 2),
              formatC(dev_lambda,  format = "e", digits = 2),
              formatC(ifelse(length(lam_kartal) == 1, lam_kartal, NA), format = "f", digits = 1)),
  ssp     = SSP,
  zaman   = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
)
write_csv(prov_tbl, file.path(DIR_TBL, "tbl_provenance_check.csv"))
cat("  Provenans dogrulamasi GECTI.\n\n")


## ---------------------------------------------------------
## Yardimci: log eksenli grafikler icin gurultu tabani sansuru
## scale_y_log10 sifir/negatif degerleri UYARI VERMEDEN duser.
## Bu fonksiyon once sansurler, sonra kac satirin dustugunu raporlar.
## ---------------------------------------------------------
censor_log <- function(df, col, floor_val = NOISE_FLOOR, etiket = col) {
  v <- df[[col]]
  n_zero  <- sum(v <= 0, na.rm = TRUE)
  n_noise <- sum(v > 0 & v < floor_val, na.rm = TRUE)
  if (n_zero + n_noise > 0) {
    cat(sprintf("    [log-sansur] %s: %d sifir, %d gurultu-alti (<%.0e) satir NA yapildi\n",
                etiket, n_zero, n_noise, floor_val))
  }
  df[[col]] <- ifelse(is.na(v) | v < floor_val, NA_real_, v)
  df
}


## =========================================================
## 1) İklim Koşulları
## =========================================================
cat(">>> 1) Sicaklik-nem profili <<<\n")

p_temp_rh <- monthly %>%
  group_by(district_id, district_label, month_name) %>%
  summarise(T_mean  = mean(temp_c, na.rm = TRUE),
            RH_mean = mean(rh,     na.rm = TRUE),
            .groups = "drop") %>%
  ggplot(aes(x = month_name, group = district_id, colour = district_id)) +
  geom_line(aes(y = T_mean, linetype = "Sıcaklık (°C)"), linewidth = 0.8) +
  geom_point(aes(y = T_mean), size = 1.5) +
  geom_line(aes(y = RH_mean / 2.5, linetype = "Bağıl Nem (%)"), alpha = 0.6) +
  scale_colour_manual(
    values = COL_DISTRICT,
    labels = DISTRICT_LABELS,
    name   = "İlçeler"
  ) +
  scale_linetype_manual(
    values = c("Sıcaklık (°C)" = "solid", "Bağıl Nem (%)" = "dashed"),
    name = "Parametre"
  ) +
  scale_y_continuous(
    name = "Ortalama Sıcaklık (°C)",
    sec.axis = sec_axis(~ . * 2.5, name = "Bağıl Nem (%)")
  ) +
  labs(x     = NULL,
       title = paste("Mevsimsel Sıcaklık ve Nem Profili —", SSP_LABEL)) +
  theme_thesis() +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    legend.margin = margin(t = 0)
  )

ggsave("fig_temp_rh.png", p_temp_rh, path = DIR_FIG,
       width = 8.5, height = 5.5, dpi = 300)


## =========================================================
## 2) λ_local Isı Haritası
## =========================================================
cat(">>> 2) λ_local ısı haritası <<<\n")

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
    name     = expression(lambda[local]~"(log"[10]*" \u00f6l\u00e7ek)"),
    guide    = guide_colorbar(
      barwidth       = 20,
      barheight      = 1.2,
      title.position = "top",
      title.hjust    = 0.5
    )
  ) +
  labs(
    x       = NULL,
    y       = NULL,
    title   = paste("Ayl\u0131k yerel (otokton) bula\u015f h\u0131z\u0131 \u2014", SSP_LABEL),
    caption = paste0(
      "\u25a0 Koyu gri: Termal e\u015fik alt\u0131 (\u03bb < 10\u207b\u2078) \u2014 otokton bula\u015f imk\u00e2ns\u0131z\n",
      "\u25a0 A\u00e7\u0131k sar\u0131: Marjinal sezon ba\u015f\u0131/sonu (10\u207b\u2075\u201310\u207b\u00b3)\n",
      "\u25a0 Turuncu\u2013k\u0131rm\u0131z\u0131: Aktif bula\u015f sezonu (\u03bb \u2265 10\u207b\u00b2)"
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
## 3) P_est Mevsimselliği
## ---------------------------------------------------------
## NOT: p_establishment_mean > 0 filtresi AGREGASYONDAN ONCE
## uygulanmaktadir; dolayisiyla aylik ortalama yalnizca o ayin aktif
## oldugu yillar uzerinden alinir. Bu bilincli bir tercihtir (termal
## olarak olu aylar mevsimsel profili yapay olarak sifira cekmesin
## diye), ancak ay basi/sonu icin ortalamanin dayandigi yil sayisi
## dusuktur. Sekil altyazisinda belirtilmelidir.
## =========================================================
cat(">>> 3) P_est mevsimselliği <<<\n")

pest_season <- monthly %>%
  filter(p_establishment_mean > 0) %>%
  group_by(district_id, month_name) %>%
  summarise(pest_mean = mean(p_establishment_mean, na.rm = TRUE),
            n_yil     = dplyr::n(),
            .groups = "drop") %>%
  censor_log("pest_mean", etiket = "P_est mevsimsel")

fig_pest_season <- ggplot(filter(pest_season, !is.na(pest_mean)),
                          aes(x = month_name, y = pest_mean,
                              group = district_id,
                              colour = district_id)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.5) +
  scale_y_log10(labels = label_scientific()) +
  scale_colour_manual(values = COL_DISTRICT, labels = DISTRICT_LABELS, name = NULL) +
  labs(x = NULL, y = expression(P[est]~"(log ölçek)"),
       title = paste("Yerleşme olasılığı mevsimsel profili —", SSP_LABEL),
       caption = "Ortalama yalnızca ilgili ayın aktif olduğu yıllar üzerinden alınmıştır.") +
  theme_thesis() +
  theme(plot.caption = element_text(size = 8, colour = "grey40", hjust = 0))

ggsave("fig_pest_season.png", fig_pest_season, path = DIR_FIG,
       width = 8, height = 5, dpi = 300)


## =========================================================
## 4) Yıllık Risk Trendi
## ---------------------------------------------------------
## v2 DUZELTMELERI:
##   (a) scale_y_log10 sifir/negatif satirlari SESSIZCE duser.
##       SSP5-8.5 / Hopa / 2028'de p_ge1_major_year_mean == 0'dir.
##       Artik censor_log() ile acikca NA yapiliyor ve raporlaniyor.
##   (b) ~1.1e-16 altindaki degerler makine epsilon artefaktidir;
##       NOISE_FLOOR ile kesiliyor ve eksen bu tabana sabitleniyor.
##   (c) Serit (ribbon) bir MONTE CARLO ORNEKLEME ARALIGIDIR; parametre
##       guven araligi degildir. Altyaziya eklendi.
## =========================================================
cat(">>> 4) Yıllık risk trendi <<<\n")

yearly_plot <- yearly %>%
  censor_log("p_ge1_major_year_mean", etiket = "yillik risk (ort)") %>%
  censor_log("p_ge1_major_year_p2_5", etiket = "yillik risk (p2.5)") %>%
  censor_log("p_ge1_major_year_p97_5", etiket = "yillik risk (p97.5)") %>%
  filter(!is.na(p_ge1_major_year_mean))

n_dropped <- nrow(yearly) - nrow(yearly_plot)

y_lo <- max(NOISE_FLOOR, min(yearly_plot$p_ge1_major_year_mean, na.rm = TRUE))
y_hi <- max(yearly_plot$p_ge1_major_year_mean, na.rm = TRUE)

cap_tr <- paste0(
  "Gölgeli bant, dış Monte Carlo tekrarlarının %2,5–%97,5 persentilidir ",
  "(örnekleme aralığı; parametre güven aralığı DEĞİLDİR).\n",
  "Sayısal gürültü tabanı 10\u207b\u00b9\u2075'tir; bu değerin altındaki hücreler ",
  "çizilmemiştir",
  if (n_dropped > 0) paste0(" (", n_dropped, " ilçe-yıl).") else "."
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
           label = "sayısal gürültü tabanı", hjust = 0, vjust = -0.6,
           size = 2.7, colour = "grey45") +
  scale_y_log10(labels = label_scientific(),
                limits = c(min(y_lo, NOISE_FLOOR), y_hi),
                breaks = 10^seq(-16, 0, 2)) +
  scale_colour_manual(values = COL_DISTRICT, labels = DISTRICT_LABELS, name = NULL) +
  scale_fill_manual(values   = COL_DISTRICT, labels = DISTRICT_LABELS, guide = "none") +
  labs(x = "Yıl", y = expression(P["≥1 majör/yıl"]~"(log"[10]*" ölçek)"),
       title = paste("Yıllık salgın riski —", SSP_LABEL),
       caption = cap_tr) +
  theme_thesis() +
  theme(plot.caption = element_text(size = 8, colour = "grey40", hjust = 0,
                                    margin = margin(t = 8)))

ggsave("fig_yearly_risk.png", fig_yearly, path = DIR_FIG,
       width = 8, height = 5, dpi = 300)

# Sansurlenen satirlarin kaydi (savunmada sorulursa)
if (n_dropped > 0) {
  yearly %>%
    filter(p_ge1_major_year_mean < NOISE_FLOOR | is.na(p_ge1_major_year_mean)) %>%
    select(district_id, year, p_ge1_major_year_mean,
           Lambda_import_year, mean_p_est_year_mean) %>%
    write_csv(file.path(DIR_TBL, "tbl_yearly_censored_rows.csv"))
}


## =========================================================
## 4-EN) Annual Risk Trend  (ENGLISH)
## =========================================================
cat(">>> 4) Annual risk trend (EN) <<<\n")

DISTRICT_LABELS_EN <- gsub("\u0130stanbul", "Istanbul", DISTRICT_LABELS)

cap_en <- paste0(
  "Shaded band = 2.5th-97.5th percentile of outer Monte Carlo replicates ",
  "(sampling interval; NOT a parameter confidence interval).\n",
  "Numerical noise floor is 10\u207b\u00b9\u2075; cells below this value are not plotted",
  if (n_dropped > 0) paste0(" (", n_dropped, " district-years).") else "."
)

fig_yearly_en <- ggplot(yearly_plot,
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
       y = expression(P["\u22651 major/year"] ~ "(log"[10]*" scale)"),
       title = paste("Annual outbreak risk \u2014", SSP_LABEL),
       caption = cap_en) +
  theme_thesis() +
  theme(plot.caption = element_text(size = 8, colour = "grey40", hjust = 0,
                                    margin = margin(t = 8)))

ggsave("fig_yearly_risk_en.png", fig_yearly_en, path = DIR_FIG,
       width = 8, height = 5, dpi = 300)


## =========================================================
## 5) Ufuk Tablosu
## =========================================================
cat(">>> 5) Ufuk risk tablosu <<<\n")

horizon_tbl <- horizon %>%
  select(district_label,
         p_mean = p_ge1_major_mean,
         p_lo = p_ge1_major_p2_5,
         p_hi = p_ge1_major_p97_5,
         Lambda = Lambda_import) %>%
  arrange(desc(p_mean)) %>%
  mutate(across(c(p_mean, p_lo, p_hi), ~formatC(.x, format = "e", digits = 2)),
         Lambda = round(Lambda, 1)) %>%
  rename(`İlçe` = district_label,
         `P_ufuk (MC ort.)` = p_mean,
         `MC %2,5` = p_lo,
         `MC %97,5` = p_hi,
         `Λ_ithalat (51 yıl)` = Lambda)

write_csv(horizon_tbl, file.path(DIR_TBL, "tbl_horizon.csv"))

ft <- flextable(horizon_tbl) %>%
  autofit() %>%
  theme_vanilla() %>%
  flextable::add_footer_lines(
    "MC %2,5 / %97,5: dış Monte Carlo örnekleme aralığı; parametre güven aralığı değildir.")
doc <- read_docx() %>% body_add_flextable(ft)
print(doc, target = file.path(DIR_TBL, "tbl_horizon.docx"))


## =========================================================
## 5b) İthalat Baskısı Özeti (λ_import trendi + kümülatif Λ)
## =========================================================
cat(">>> 5b) İthalat baskısı özeti <<<\n")

# ---- 5b.1) Yıllık ithalat baskısı trendi (log ölçek) ----
import_plot <- censor_log(yearly, "Lambda_import_year", etiket = "Lambda_yillik") %>%
  filter(!is.na(Lambda_import_year))

fig_import_trend <- ggplot(import_plot,
                           aes(x = year, y = Lambda_import_year,
                               colour = district_id)) +
  geom_line(linewidth = 0.7) +
  geom_smooth(method = "loess", formula = y ~ x, se = FALSE, linewidth = 0.5,
              linetype = "dashed", alpha = 0.5) +
  scale_y_log10(labels = label_scientific()) +
  scale_colour_manual(values = COL_DISTRICT,
                      labels = DISTRICT_LABELS,
                      name = NULL) +
  labs(x = "Yıl",
       y = expression(Lambda["ithalat, yıllık"]~"(log ölçek)"),
       title = paste("Yıllık ithalat baskısı —", SSP_LABEL)) +
  theme_thesis()

ggsave("fig_import_trend.png", fig_import_trend, path = DIR_FIG,
       width = 8, height = 5, dpi = 300)

# ---- 5b.2) Aylık ithalat baskısı ısı haritası ----
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
                       name = expression(lambda["ithalat"])) +
  labs(x = NULL, y = NULL,
       title = paste("Aylık ortalama ithalat baskısı —", SSP_LABEL)) +
  theme_thesis() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("fig_import_heatmap.png", fig_import_heat, path = DIR_FIG,
       width = 8, height = 4, dpi = 300)

# ---- 5b.3) Kümülatif Λ_import özet tablosu ----
import_summary <- horizon %>%
  select(district_label, Lambda_import) %>%
  arrange(desc(Lambda_import)) %>%
  mutate(
    Lambda_fmt = round(Lambda_import, 1),
    Yorum = case_when(
      Lambda_import >= 100 ~ "Yüksek ithalat baskısı",
      Lambda_import >= 10  ~ "Orta ithalat baskısı",
      Lambda_import >= 1   ~ "Düşük ithalat baskısı",
      TRUE                 ~ "Çok düşük (< 1 beklenen vaka/51 yıl)"
    )
  ) %>%
  select(`İlçe` = district_label,
         `Λ_ithalat (51 yıl)` = Lambda_fmt,
         Yorum)

write_csv(import_summary, file.path(DIR_TBL, "tbl_import_summary.csv"))

ft_imp <- flextable(import_summary) %>% autofit() %>% theme_vanilla()
doc_imp <- read_docx() %>% body_add_flextable(ft_imp)
print(doc_imp, target = file.path(DIR_TBL, "tbl_import_summary.docx"))

cat("  İthalat baskısı grafikleri ve tablosu kaydedildi.\n")


## =========================================================
## 6) AŞAMA 2 — Tahmin Edici Karşılaştırması (sigma_EIP > 0)
## ---------------------------------------------------------
## v2 NOTU: v1'de bu bolum "Model dogrulamasi" olarak adlandiriliyordu.
## Ancak burada karsilastirilan iki nicelik sunlardir:
##   - p_establishment_mean = E[P_est(EIP)]   (stokastik EIP, birincil)
##   - P_est_analytic       = P_est(E[EIP])   (yerine-koyma)
## Bu, tezdeki ASAMA 2'dir (tahmin edici karsilastirmasi).
## ASAMA 1 (yazilim dogrulamasi) sigma_EIP = 0 ile ayri bir kosu
## gerektirir ve bu betikte YAPILMAZ; bkz. validation_two_stage.R.
## =========================================================
cat(">>> 6) Aşama 2 — tahmin edici karşılaştırması <<<\n")

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

# Jensen yonu: tez, TUM aktif hucrelerde P_est(E[EIP]) > E[P_est(EIP)]
# oldugunu iddia eder. Burada dogrulaniyor.
n_yon_ihlali <- sum(val_active$signed_diff < 0, na.rm = TRUE)

val_summary <- tibble(
  Metrik = c("Toplam kombinasyon", "Aktif (λ>0)",
             "Ort. |fark|", "Maks |fark|", "< 0.02 eşiği",
             "Jensen yön ihlali (P_est(E[EIP]) < E[P_est(EIP)])"),
  Değer = c(nrow(val_df), nrow(val_active),
            formatC(mean(val_active$abs_diff), format = "e", digits = 2),
            formatC(max(val_active$abs_diff), format = "e", digits = 2),
            paste0(sum(val_active$abs_diff < 0.02), "/", nrow(val_active)),
            paste0(n_yon_ihlali, "/", nrow(val_active)))
)

write_csv(val_summary, file.path(DIR_TBL, "tbl_validation.csv"))

if (n_yon_ihlali > 0)
  warning(sprintf("[ASAMA 2] %d aktif hucrede Jensen yonu tersine donmus. Tez metni\n",
                  n_yon_ihlali),
          "  'yon hicbir hucrede tersine donmemistir' diyor; guncellenmelidir.",
          call. = FALSE)

val_plot <- val_active %>%
  censor_log("P_est_analytic",       etiket = "P_est analitik") %>%
  censor_log("p_establishment_mean", etiket = "P_est MC") %>%
  filter(!is.na(P_est_analytic), !is.na(p_establishment_mean))

fig_val <- ggplot(val_plot, aes(x = P_est_analytic,
                                y = p_establishment_mean,
                                colour = district_label)) +
  geom_point(alpha = 0.4, size = 1) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "red") +
  scale_x_log10(labels = label_scientific()) +
  scale_y_log10(labels = label_scientific()) +
  scale_colour_manual(values = COL_DISTRICT_LABEL, name = NULL) +
  labs(x = expression(P[est](E*"["*EIP*"]")~" (yerine-koyma)"),
       y = expression(E*"["*P[est](EIP)*"]"~" (birincil, MC)"),
       title = paste("Aşama 2 — tahmin edici karşılaştırması —", SSP_LABEL),
       caption = paste0("Gürültü tabanı 10\u207b\u00b9\u2075 altındaki hücreler çizilmemiştir. ",
                        "Bu bir yazılım doğrulaması (Aşama 1) değildir.")) +
  theme_thesis() +
  theme(plot.caption = element_text(size = 8, colour = "grey40", hjust = 0))

ggsave("fig_validation.png", fig_val, path = DIR_FIG,
       width = 7, height = 5, dpi = 300)


## =========================================================
## 7) İthalat Duyarlılık (k + η)
## =========================================================
cat(">>> 7) İthalat duyarlılığı <<<\n")

sens_imp <- load_sens_imp(SSP)
if (!is.null(sens_imp)) {
  write_csv(sens_imp, file.path(DIR_TBL, "tbl_sens_importation.csv"))
  cat("  İthalat duyarlılık tablosu kaydedildi.\n")
} else {
  cat("  İthalat duyarlılık verisi bulunamadı — atlanıyor.\n")
}


## =========================================================
## 8) Model Duyarlılık (m, beta_vh, IP)
## =========================================================
cat(">>> 8) Model duyarlılığı <<<\n")

sens_mc <- load_sens_mc(SSP)
tornado_path <- file.path(here("outputs", SSP, "sensitivity", "ctmc_mc"),
                          "sensitivity_tornado.csv")

if (!is.null(sens_mc) && file.exists(tornado_path)) {
  sens_tor <- read_csv(tornado_path, show_col_types = FALSE) %>%
    mutate(
      signed_log10 = sign(mean_delta_pct) * log10(abs(mean_delta_pct) + 1),
      senaryo_tr   = dplyr::recode(scenario, !!!SCEN_TR)
    )

  max_abs <- max(abs(sens_tor$signed_log10), na.rm = TRUE) * 1.1

  fig_tornado <- ggplot(sens_tor,
                        aes(x = reorder(senaryo_tr, abs(signed_log10)),
                            y = signed_log10)) +
    geom_col(aes(fill = signed_log10 > 0), show.legend = FALSE, width = 0.7) +
    geom_text(aes(label = sprintf("%+.0f%%", mean_delta_pct),
                  hjust = ifelse(signed_log10 >= 0, -0.1, 1.1)),
              size = 3, colour = "grey30") +
    coord_flip(ylim = c(-max_abs, max_abs)) +
    scale_fill_manual(values = c("TRUE" = "#E63946", "FALSE" = "#457B9D")) +
    labs(x = NULL,
         y = expression("Baz senaryoya göre değişim — sign(x) " %*% " log"[10]*"(|%Δ|+1)"),
         title = paste("Parametre duyarlılığı —", SSP_LABEL),
         caption = "Çubuk yanındaki etiketler orijinal % değişimi gösterir") +
    theme_thesis() +
    theme(axis.text.y = element_text(size = 9))

  ggsave("fig_tornado.png", fig_tornado, path = DIR_FIG,
         width = 8, height = 5, dpi = 300)
  cat("  Tornado grafiği (log10) kaydedildi.\n")
} else {
  cat("  Model duyarlılık verisi bulunamadı — atlanıyor.\n")
}


## =========================================================
## 9) GBD Ülke Katkıları
## =========================================================
cat(">>> 9) GBD ülke katkıları <<<\n")

cc <- load_country_contrib(SSP)
if (!is.null(cc)) {
  top_25 <- cc %>% slice_max(contribution_pct, n = 25)

  fig_country <- ggplot(top_25,
                        aes(x = reorder(country, contribution_pct),
                            y = contribution_pct)) +
    geom_col(fill = "#E63946", alpha = 0.85) +
    coord_flip() +
    labs(x = NULL, y = "İthalat riskine katkı (%)",
         title = "Kaynak ülke bazlı dang ithalat riski",
         subtitle = paste("GBD 2023 × turist ağırlığı —", SSP_LABEL)) +
    theme_thesis()

  ggsave("fig_country_contrib.png", fig_country, path = DIR_FIG,
         width = 7, height = 5, dpi = 300)

  write_csv(top_25, file.path(DIR_TBL, "tbl_country_contrib.csv"))

  # Tez metnindeki "Brezilya ~%40 / %42,6" tutarsizligi icin kanonik deger
  br <- cc %>% filter(grepl("^Brazil|Brezilya", country, ignore.case = TRUE))
  if (nrow(br) == 1)
    cat(sprintf("  >> Brezilya katki payi = %%%.1f  (tez metninde TEK degere sabitlenmeli)\n",
                br$contribution_pct[1]))

  cat("  Ülke katkısı grafiği ve tablosu kaydedildi.\n")
} else {
  cat("  Ülke katkısı verisi bulunamadı — atlanıyor.\n")
}


## =========================================================
## 10) LHS–PRCC Duyarlılık Analizi (SSP-spesifik iklim verisi)
## ---------------------------------------------------------
## !!! DIKKAT — TEZ METNIYLE CELISKI !!!
##
## Bu bolumun PRCC CIKTI DEGISKENI R0'DIR, P_ufuk DEGILDIR.
## compute_R0_lhs() bir R0 degeri uretir ve PRCC bu R0'a karsi alinir.
## P_ufuk hesabi ithalat baskisi, 612 aylik birikim ve sonlu esik
## dinamigi gerektirir; bu bolumde bunlarin hicbiri yapilmaz.
##
## Tez metnindeki su ifadeler bu kodla UYUSMAMAKTADIR:
##   - Bolum 6.9: "Model ciktisi olan P_ufuk uzerindeki degiskenligin..."
##   - Sekil 6.9 altyazisi: "...her girdinin P_ufuk ile kismi siralama
##     korelasyon katsayisini..."
##   - Ek A.5 (guncel surumde): "PRCC, P_ufuk ciktisi temel alinarak..."
##
## Dogru ifade R0'dir. Ya (a) tez metninin bu uc yeri R0 olarak
## duzeltilmeli, ya da (b) analiz gercekten P_ufuk uzerinde yeniden
## kurulmalidir. (a) tercih edilirse Ek A.5'in ORIJINAL ifadesi
## ("R0 ciktisi temel alinarak") dogruydu.
##
## Ayrica: bu bolum butun hucreler icin Ae. albopictus termal
## parametrelerini kullanir; tur-spesifik degildir (Hopa ve Zonguldak
## modelde Ae. aegypti ile calisilmaktadir). Ek A.3'teki CSI uyarisinin
## bir benzeri burada da gereklidir.
##
## v2'DE DUZELTILEN: guven araligi artik gercekten bootstrap'tir.
## v1 cor.test()$conf.int (Fisher z donusumu) kullaniyordu; tez metni
## "%95 bootstrap guven araliklari" diyor. Nokta kestirimleri AYNIDIR.
## =========================================================
cat(">>> 10) LHS–PRCC duyarlılık analizi (çıktı = R0) <<<\n")
cat("  [UYARI] PRCC ciktisi R0'dir. Tez metninde 'P_ufuk' yazan yerler\n")
cat("          (Bolum 6.9, Sekil 6.9 altyazisi, Ek A.5) duzeltilmelidir.\n")

library(lhs)

set.seed(123)
n_lhs   <- 2000
n_boot  <- 2000

# ---- SSP-spesifik: T ve RH degerlerini gercek iklim verisinden ornekle ----
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

cat("  SSP-spesifik iklim (bootstrap örnekleme):\n")
cat("    T_C  :", T_range[1], "–", T_range[2], "°C\n")
cat("    RH   :", RH_range[1], "–", RH_range[2], "%\n")
cat("    Aktif ay havuzu:", nrow(active_climate), "gözlem\n")

# ---- Diger parametreler: LHS ile uniform ornekleme ----
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

# Mordecai 2017 Ae. albopictus parametreleri (TUM hucreler icin; bkz. uyari)
C_a   <- 1.93e-4; T0_a  <- 10.25; Tm_a  <- 38.32
C_eip <- 1.09e-4; T0_e  <- 10.39; Tm_e  <- 43.05
C_lf  <- 1.43e-1; T0_lf <- 6.24;  Tm_lf <- 38.25
k_vpd <- 0.5; SVPD_ref <- 1.0; lf_floor <- 0.25

briere_fn  <- function(T, c, T0, Tm) ifelse(T > T0 & T < Tm, c * T * (T - T0) * sqrt(Tm - T), 0)
quad_lf_fn <- function(T, c, T0, Tm) pmax(c * (T - T0) * (Tm - T), lf_floor)
svpd_fn    <- function(T, RH) pmax(0.6108 * exp(17.27 * T / (T + 237.3)) * (1 - RH / 100), 0)

# v2: vektorize edildi (v1 apply() ile satir satir donuyordu; yavas ve
# lhs_params'a yeni sutun eklenirse kirilgan)
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

# Tek bir PRCC degeri hesaplayan cekirdek (bootstrap icinde de kullanilir)
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

  # --- v2: gercek bootstrap guven araligi ---
  boot_vals <- vapply(seq_len(n_boot), function(b) {
    idx <- sample.int(nrow(lhs_params), replace = TRUE)
    prcc_one(lhs_params[idx, , drop = FALSE], nm)
  }, numeric(1))

  ci <- stats::quantile(boot_vals, c(0.025, 0.975), na.rm = TRUE)

  # p-degeri: parametrik (Fisher z) referans olarak korunuyor
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
cat("  PRCC siralamasi (cikti = R0):\n")
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
               "Bootstrap (aktif ay havuzu)", "Bootstrap (aktif ay havuzu)"),
  Source = c("Literature", "Literature", "Literature", "Literature",
             paste0("Bootstrap (", SSP_LABEL, " active months)"),
             paste0("Bootstrap (", SSP_LABEL, " active months)"))
)

write_csv(param_ranges_tbl, file.path(DIR_TBL, "tbl_param_ranges.csv"))

fig_prcc <- ggplot(prcc_result, aes(x = reorder(parameter, abs(PRCC)), y = PRCC)) +
  geom_col(aes(fill = PRCC > 0), show.legend = FALSE, width = 0.7) +
  geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi), width = 0.2) +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = "#E63946", "FALSE" = "#457B9D")) +
  labs(x = NULL, y = expression("PRCC ("*R[0]*" \u00e7\u0131kt\u0131s\u0131)"),
       title = paste(SSP_LABEL, "LHS\u2013PRCC duyarl\u0131l\u0131k analizi"),
       subtitle = sprintf("n = %d | T: %.1f\u2013%.1f\u00b0C | RH: %.1f\u2013%.1f%% (aktif-ay bootstrap)",
                          n_lhs, T_range[1], T_range[2],
                          RH_range[1], RH_range[2]),
       caption = paste0("Çıktı değişkeni R\u2080'dır (P_ufuk değil). ",
                        "Hata çubukları %95 bootstrap güven aralığıdır (B = ", n_boot, ").\n",
                        "Termal parametreler tüm örneklemler için Ae. albopictus'a aittir.")) +
  theme_thesis() +
  theme(plot.caption = element_text(size = 8, colour = "grey40", hjust = 0))

ggsave("fig_prcc.png", fig_prcc, path = DIR_FIG, width = 7, height = 4.8, dpi = 300)


## =========================================================
## 11) CSI Isı Haritası ve Trend
## ---------------------------------------------------------
## NOT: CSI tur-notr bir karsilastirma olcutu olarak yalnizca
## Ae. albopictus egrileriyle hesaplanmaktadir. Modelde Ae. aegypti
## atanan Hopa ve Zonguldak icin CSI ile P_est arasinda birebir tur
## uyumu YOKTUR. (Tez Ek A.3'te bu uyari mevcuttur.)
## =========================================================
cat(">>> 11) CSI ısı haritası ve trend <<<\n")

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
    name    = "İklim Uygunluk\nİndeksi (CSI)",
    limits  = c(0, 1),
    breaks  = c(0, 0.25, 0.50, 0.75, 1.00),
    labels  = c("0,00", "0,25", "0,50", "0,75", "1,00"),
    guide   = guide_colorbar(barwidth = 18, barheight = 1.0,
                             title.position = "top", title.hjust = 0.5,
                             label.hjust = 0.5)
  ) +
  labs(
    x       = "Ay",
    y       = "İlçe",
    title   = paste("İklim Uygunluk İndeksi \u2014", SSP_LABEL),
    caption = paste0("CSI = (a_norm + lf_norm + eip_norm) / 3; Bri\u00e8re termal ",
                     "performans e\u011frileri, Mordecai 2017.\n",
                     "CSI t\u00fcm il\u00e7eler i\u00e7in Ae. albopictus e\u011frileriyle ",
                     "hesaplanm\u0131\u015ft\u0131r; Hopa ve Zonguldak modelde Ae. aegypti ile ",
                     "\u00e7al\u0131\u015f\u0131lmaktad\u0131r.")
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
  scale_colour_manual(values = COL_DISTRICT, labels = DISTRICT_LABELS, name = NULL) +
  scale_fill_manual(values = COL_DISTRICT, labels = DISTRICT_LABELS, guide = "none") +
  labs(x = "Yıl", y = "Ortalama CSI",
       title = paste("CSI yıllık trendi \u2014", SSP_LABEL)) +
  theme_thesis()

ggsave("fig_csi_trend.png", fig_csi_trend, path = DIR_FIG,
       width = 8, height = 5, dpi = 300)

write_csv(csi_heat, file.path(DIR_TBL, "tbl_csi_monthly.csv"))

# Tez Tartisma bolumundeki "pik ayda Fethiye 0,83 > Kartal 0,79" iddiasi
csi_peak <- csi_heat %>%
  group_by(district_label) %>%
  slice_max(CSI_mean, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(desc(CSI_mean))
write_csv(csi_peak, file.path(DIR_TBL, "tbl_csi_peak.csv"))
cat("  Pik ay CSI degerleri (tez Tartisma bolumu ile karsilastirin):\n")
print(as.data.frame(csi_peak %>% mutate(CSI_mean = round(CSI_mean, 3))),
      row.names = FALSE)


## =========================================================
## 12) Moran's I Mekânsal Otokorelasyon  (kesifsel)
## ---------------------------------------------------------
## n = 5 ile Moran's I'in istatistiksel gucu son derece dusuktur;
## sonuc tezde raporlanmiyorsa kesifsel kalmalidir.
## =========================================================
cat(">>> 12) Moran's I (keşifsel, n=5) <<<\n")

if (requireNamespace("spdep", quietly = TRUE) &&
    requireNamespace("sf", quietly = TRUE) &&
    requireNamespace("ggrepel", quietly = TRUE)) {

  library(spdep); library(sf)

  # Kanonik ilce-koordinat eslemesi (00_results_setup.R ile ayni)
  district_coords <- data.frame(
    district_id = c("TUR.10.4_1","TUR.39.3_1","TUR.40.25_1","TUR.59.4_1","TUR.81.6_1"),
    label = c("Hopa","Eğirdir","Kartal","Fethiye","Zonguldak"),
    lon = c(41.12, 30.85, 29.19, 29.12, 31.79),
    lat = c(41.41, 37.88, 40.89, 36.62, 41.45)
  )

  # Eslemenin setup dosyasiyla tutarliligini dogrula (etiket takasi guvenligi)
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
    Değişken = c("log₁₀(p_ge1_major)", "CSI"),
    `Moran's I` = c(round(mt_p$estimate[1], 4), round(mt_csi$estimate[1], 4)),
    `p-değeri` = c(format.pval(mt_p$p.value, digits = 3),
                   format.pval(mt_csi$p.value, digits = 3)),
    Yorum = c(ifelse(mt_p$p.value < 0.05, "Anlamlı kümelenme",
                     "Anlamlı otokorelasyon yok (n=5, güç sınırlı)"),
              ifelse(mt_csi$p.value < 0.05, "Anlamlı kümelenme",
                     "Anlamlı otokorelasyon yok (n=5, güç sınırlı)"))
  )

  write_csv(moran_tbl, file.path(DIR_TBL, "tbl_moran.csv"))

  z_p     <- scale(log10(pmax(horizon_vec, NOISE_FLOOR)))[,1]
  z_csi   <- scale(csi_vec)[,1]
  lag_p   <- lag.listw(w2, z_p)
  lag_csi <- lag.listw(w2, z_csi)

  moran_df <- tibble(label = district_coords$label,
                     z_p = z_p, lag_p = lag_p,
                     z_csi = z_csi, lag_csi = lag_csi)

  p_moran <- ggplot(moran_df, aes(x = z_p, y = lag_p)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey60") +
    geom_vline(xintercept = 0, linetype = "dashed", colour = "grey60") +
    geom_smooth(method = "lm", formula = y ~ x, se = TRUE,
                colour = "#E63946", alpha = 0.15) +
    geom_point(size = 4, colour = "#E63946", shape = 21, fill = "white", stroke = 1.5) +
    ggrepel::geom_text_repel(aes(label = label), size = 3.2) +
    labs(x = "Std. log₁₀(p_ge1_major)", y = "Mekânsal gecikme",
         title = paste("Moran saçılımı —", SSP_LABEL),
         caption = "Keşifsel: n = 5, istatistiksel güç sınırlıdır.") +
    theme_thesis() +
    theme(plot.caption = element_text(size = 8, colour = "grey40", hjust = 0))

  ggsave("fig_moran.png", p_moran, path = DIR_FIG, width = 6, height = 5, dpi = 300)
  cat("  Moran's I tamamlandı.\n")
} else {
  cat("  spdep/sf/ggrepel paketi yok — Moran's I atlanıyor.\n")
}


## =========================================================
## 13) Dekadal risk progresyonu
## =========================================================
cat(">>> 13) Dekadal risk progresyonu <<<\n")

decade_df <- yearly %>%
  mutate(
    dekad_yil = floor(year / 10) * 10,
    dekad = factor(
      dekad_yil,
      levels = seq(2020, 2070, 10),
      labels = c("2020'ler","2030'lar","2040'lar",
                 "2050'ler","2060'lar","2070'ler")
    )
  ) %>%
  filter(!is.na(dekad)) %>%
  group_by(district_id, district_label, dekad) %>%
  summarise(p_ort = mean(p_ge1_major_year_mean, na.rm = TRUE), .groups = "drop") %>%
  censor_log("p_ort", etiket = "dekadal ortalama risk")

dec_lo <- max(NOISE_FLOOR, min(decade_df$p_ort, na.rm = TRUE))

fig_decade <- ggplot(filter(decade_df, !is.na(p_ort)),
                     aes(x = dekad, y = p_ort,
                         colour = district_id, group = district_id)) +
  geom_line(linewidth = 0.8, alpha = 0.7) +
  geom_point(size = 3) +
  scale_colour_manual(values = COL_DISTRICT, labels = DISTRICT_LABELS,
                      name = "\u0130l\u00e7e") +
  scale_y_log10(
    labels = label_scientific(digits = 1),
    breaks = 10^seq(-16, 0, 2),
    limits = c(dec_lo, NA)
  ) +
  labs(
    title = paste("On y\u0131ll\u0131k risk progresyonu \u2014", SSP_LABEL),
    x     = "On Y\u0131l",
    y     = "Ort. y\u0131ll\u0131k risk (log\u2081\u2080)"
  ) +
  theme_thesis() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

ggsave("fig_decade.png", fig_decade, path = DIR_FIG,
       width = 9, height = 5, dpi = 300)

decade_wide <- decade_df %>%
  select(district_label, dekad, p_ort) %>%
  pivot_wider(names_from = dekad, values_from = p_ort) %>%
  rename("\u0130l\u00e7e" = district_label)

write_csv(decade_wide, file.path(DIR_TBL, "tbl_decade.csv"))
cat("  tbl_decade.csv yazıldı:", unique(yearly$ssp), "\n")


## =========================================================
## 14) Bulaş Sezonu Uzunluğu Değişimi
## ---------------------------------------------------------
## v2 DUZELTMESI: v1'de eksen etiketi "λ_local > 0" diyordu, oysa hesap
## LAMBDA_THRESH = 1e-4 ile yapiliyordu. Tez tanimi da 1e-4'tur
## ("bulas sezonu = lambda_local > 10^-4 olan ay sayisi"). Etiket
## hesapla ve tezle uyumlu hale getirildi.
## =========================================================
cat(">>> 14) Bulaş sezonu uzunluğu <<<\n")

LAMBDA_THRESH <- 1e-4

season_yr <- monthly %>%
  group_by(district_id, district_label, year) %>%
  summarise(
    season_len = sum(lambda_local_i1_mean > LAMBDA_THRESH, na.rm = TRUE),
    peak_month = AY_TR[which.max(lambda_local_i1_mean)],
    .groups = "drop"
  )

fig_season <- ggplot(season_yr, aes(x = year, y = season_len,
                                    colour = district_id, fill = district_id)) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, alpha = 0.15, linewidth = 1.2) +
  scale_colour_manual(values = COL_DISTRICT, labels = DISTRICT_LABELS, name = "İlçe") +
  scale_fill_manual(values = COL_DISTRICT, labels = DISTRICT_LABELS, name = "İlçe") +
  scale_y_continuous(breaks = 0:12) +
  labs(title = paste("Bulaş sezonu uzunluğu —", SSP_LABEL),
       x = "Yıl",
       y = expression("Aktif ay sayısı ("*lambda[local]*" > 10"^-4*")")) +
  theme_thesis()

ggsave("fig_season.png", fig_season, path = DIR_FIG, width = 8, height = 5, dpi = 300)

season_comp <- season_yr %>%
  mutate(donem = case_when(
    year >= 2025 & year <= 2035 ~ "Erken",
    year >= 2065 & year <= 2075 ~ "Geç",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(donem)) %>%
  group_by(district_label, donem) %>%
  summarise(sezon_ort = mean(season_len, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = donem, values_from = sezon_ort) %>%
  mutate(
    Erken = round(Erken, 1),
    Geç   = round(Geç,   1),
    delta = round(Geç - Erken, 1)
  )

write_csv(season_comp, file.path(DIR_TBL, "tbl_season.csv"))

# Pik ay kaymasi (tez Tartisma bolumundeki "Haziran-Ekim -> Mayis-Kasim"
# iddiasi icin dogrulanabilir bir dayanak)
peak_shift <- season_yr %>%
  mutate(donem = case_when(
    year >= 2025 & year <= 2035 ~ "Erken",
    year >= 2065 & year <= 2075 ~ "Geç",
    TRUE ~ NA_character_)) %>%
  filter(!is.na(donem)) %>%
  count(district_label, donem, peak_month) %>%
  group_by(district_label, donem) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup()
write_csv(peak_shift, file.path(DIR_TBL, "tbl_peak_month_shift.csv"))


## =========================================================
## 15) Çoklu Regresyon — YILLIK RİSK üzerine (KEŞİFSEL)
## ---------------------------------------------------------
## !!! BU BOLUM TEZIN 6.10 REGRESYON TABLOLARINI URETMEZ !!!
##
## Bagimli degisken burada log10(p_ge1_major_year_mean)'dir.
## Tezin 6.10 bolumu ise log10(P_est) uzerine kuruludur ve kanonik
## kaynagi 6_10_pest_regression.R'dir. Dosya adi cakismasini onlemek
## icin ciktilar "_yearlyrisk" son ekiyle yazilir.
##
## v2 DUZELTMESI — T_opt artik M2'den hesaplaniyor:
## v1, log10(Lambda_import + 1) iceren M3'ten T_opt turetiyordu.
## Lambda_import sicaklikla guclu esdogrusaldir (M_climate araciligiyla)
## ve bu, tezde gecersiz sayilan "OLS T_opt ~ 19 C" artefaktini uretir.
## Tezin kanonik OLS T_opt degeri (25,6 C) M2'den gelir.
## =========================================================
cat(">>> 15) Çoklu regresyon — yıllık risk (KEŞİFSEL) <<<\n")

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

  # --- T_opt: M2'den (Lambda_import DAHIL DEGIL) ---
  b1_m2 <- coef(lm2)["T_season"]
  b2_m2 <- coef(lm2)["I(T_season^2)"]
  T_opt_m2 <- -b1_m2 / (2 * b2_m2)

  # --- Karsilastirma icin M3'ten (esdogrusallik artefakti; RAPORLAMAYIN) ---
  b1_m3 <- coef(lm3)["T_season"]
  b2_m3 <- coef(lm3)["I(T_season^2)"]
  T_opt_m3 <- -b1_m3 / (2 * b2_m3)

  cat(sprintf("  T_opt (M2, raporlanabilir)          : %.1f °C\n", T_opt_m2))
  cat(sprintf("  T_opt (M3, Lambda dahil — ARTEFAKT) : %.1f °C  [raporlamayin]\n",
              T_opt_m3))

  if (abs(T_opt_m2 - T_opt_m3) > 2)
    cat("  [NOT] M2 ile M3 optimumlari 2 C'den fazla ayrisiyor; bu, Lambda_import\n",
        "        ile sicaklik arasindaki esdogrusalligin gostergesidir.\n")

  reg_comp <- tibble(
    Model = c("M1: T + T²", "M2: + RH", "M3: + Λ_import (eşdoğrusal)"),
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
      Terim = case_when(
        term == "(Intercept)"   ~ "Sabit",
        term == "T_season"      ~ "T_sezon",
        term == "I(T_season^2)" ~ "T_sezon²",
        term == "RH_season"     ~ "RH_sezon",
        TRUE                    ~ term
      ),
      Katsayı = round(estimate, 4),
      SE = round(std.error, 4),
      p = format.pval(p.value, digits = 3)
    )

  write_csv(m2_coef, file.path(DIR_TBL, "tbl_regression_coef_yearlyrisk.csv"))
  cat("  R² (M2):", round(summary(lm2)$r.squared, 3),
      "| n =", nrow(stat_df), "\n")
  cat("  [HATIRLATMA] Bu tablolar tezin 6.10 bolumunu URETMEZ.\n")
  cat("               Kanonik kaynak: 6_10_pest_regression.R\n")
} else {
  cat("  Yetersiz veri — regresyon atlanıyor.\n")
}


## =========================================================
## 16) Karma Doğrusal Model — AYLIK MAJÖR OLASILIK üzerine (KEŞİFSEL)
## ---------------------------------------------------------
## !!! BU BOLUM TEZIN Tablo 6.10.2-6.10.6'SINI URETMEZ !!!
##
## Farklar:
##   - Bagimli degisken: burada log10(p_month_major_mean);
##     tezde log10(P_est).
##   - Bu bolumdeki ICC, log_lam (Lambda_import) ICEREN mm2'den gelir;
##     tezin ICC'si (0,839 / 0,843) log_lam ICERMEYEN LMM1'den gelir.
##   Bu iki farkin birlesimi, tezde iki farkli ICC/R2 setinin dolasmasinin
##   en olasi kaynagidir. Konsola basilan ICC degerini TEZE TASIMAYIN.
##
## v2 DUZELTMELERI:
##   - v1'de anova(), tidy(), VarCorr() satirlari ciplak birakilmisti;
##     source() icinde ust duzey ifadeler otomatik yazdirilmaz, dolayisiyla
##     bu satirlar HICBIR CIKTI URETMIYORDU. print()'e alindi.
##   - Sonuclar artik diske yaziliyor.
##   - Bolum, DONE bannerinin ONUNE tasindi.
##   - Tezle karsilastirilabilir olmasi icin log_lam ICERMEYEN bir
##     referans model (mm0) eklendi.
## =========================================================
cat(">>> 16) Karma doğrusal model — aylık p_major (KEŞİFSEL) <<<\n")

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

  cat("  n (aylık gözlem) =", nrow(stat_monthly), "\n")

  # mm0: tezin LMM1 SPESIFIKASYONUNA en yakin referans (log_lam YOK)
  mm0 <- lmer(log_p ~ temp_c + I(temp_c^2) + rh + (1 | district_id),
              data = stat_monthly, REML = TRUE)
  # mm1/mm2: ithalat baskisi eklenmis kesifsel modeller
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
    Model = c("mm0: T+T²+RH+(1|ilçe)  [Λ YOK]",
              "mm1: + log Λ_import",
              "mm2: + rastgele eğim"),
    n     = nrow(stat_monthly),
    AIC   = c(AIC(mm0), AIC(mm1), AIC(mm2)),
    T_opt = c(T_opt_of(mm0), T_opt_of(mm1), T_opt_of(mm2)),
    ICC   = c(icc_of(mm0), icc_of(mm1), icc_of(mm2))
  ) %>%
    mutate(across(c(AIC, T_opt, ICC), ~round(.x, 4)))

  write_csv(lmm_summary, file.path(DIR_TBL, "tbl_lmm_monthly_pmajor.csv"))

  cat("  Karma model ozeti (bagimli degisken = log10(p_ay,major)):\n")
  print(as.data.frame(lmm_summary), row.names = FALSE)

  cat("\n  Model karsilastirmasi (ML ile yeniden uydurulur):\n")
  print(anova(mm0, mm1, mm2))

  coef_tbl <- broom.mixed::tidy(mm1, conf.int = TRUE, effects = "fixed")
  write_csv(coef_tbl, file.path(DIR_TBL, "tbl_lmm_monthly_pmajor_coef.csv"))
  cat("\n  mm1 sabit etkiler:\n")
  print(as.data.frame(coef_tbl), row.names = FALSE)

  cat("\n  [UYARI] Buradaki ICC ve T_opt degerleri tezin Tablo 6.10.4/6.10.6\n")
  cat("          degerleri DEGILDIR (farkli bagimli degisken; mm1/mm2'de\n")
  cat("          Lambda_import mevcut). Teze TASIMAYIN.\n")
  cat("          Kanonik kaynak: 6_10_pest_regression.R\n")

} else {
  cat("  lme4 / broom.mixed bulunamadı — karma model atlanıyor.\n")
}


## =========================================================
## BİTTİ
## =========================================================
cat("\n", strrep("=", 55), "\n")
cat("DONE:", SSP_LABEL, "\n")
cat("Figures:", DIR_FIG, "\n")
cat("Tables:", DIR_TBL, "\n")
cat(strrep("=", 55), "\n")
