# ==============================================================================
# 14_restructure_04_results.R
#
# AMAC:
#   R/04_results/ klasorunu tezin genel pipeline sirasina (01_setup -> 02_data
#   -> 03_models -> 04_results) gore denetlemek; kullanilmayan/eskimis
#   betikleri arsivlemek (SILMEZ, tasir) ve klasoru daha kompakt/anlasilir
#   alt klasorlere ayirmak icin bir PLAN uretmek.
#
#   Bu betik SADECE R/04_results/ (ve onun diagnostics/ alt klasoru) icinde
#   calisir. Projenin geri kalanina (R/01_setup, R/02_data, R/03_models,
#   R/00_maintenance/13_build_clean_release.R sureci vb.) DOKUNMAZ - o ayri
#   ve hala bekleyen bir konudur.
#
#   GUVENLIK MODELI (bu projedeki diger temizlik betikleriyle ayni ilke):
#     - Hicbir dosya SILINMEZ. "Arsivleme" = R/04_results/_archive/ altina
#       TASIMA (file.rename). Istenirse geri alinabilir.
#     - DRY_RUN <- TRUE iken hicbir dosya sistemi degisikligi yapilmaz;
#       sadece ne yapilacagi konsola yazdirilir.
#     - Once DRY_RUN = TRUE ile calistirip ciktiyi incele, sonra
#       DRY_RUN = FALSE yaparak gercek tasimayi calistir.
#     - Alt klasorlere tam yeniden organizasyon (figures/, sensitivity/,
#       regression/ vb.) bu betikte YAPILMAZ; sadece plan olarak yazdirilir.
#       Neden: bu 25+ betigin source()/here() cagrilarinin tumu tek tek
#       dogrulanmadan dosyalari tasimak calisan bir pipeline'i kirabilir.
#       Asagida "FAZ 2 - ONERILEN ALT KLASOR YAPISI" bolumune bakin.
#
# KULLANIM:
#   1) Bu dosyayi R/00_maintenance/14_restructure_04_results.R olarak
#      projene koy (numaralandirma 13_build_clean_release.R ile tutarli).
#   2) RESULTS_DIR asagida projenin kokune gore ayarli; gerekirse duzelt.
#   3) DRY_RUN <- TRUE ile calistir, konsol ciktisini oku.
#   4) Sonuc mantikliysa DRY_RUN <- FALSE yap, tekrar calistir.
#
# ==============================================================================

DRY_RUN <- TRUE   # <-- once TRUE ile calistir, ciktiyi incele, sonra FALSE yap

# Proje kokune gore R/04_results yolu (gerekirse duzelt)
RESULTS_DIR <- if (requireNamespace("here", quietly = TRUE)) {
  here::here("R", "04_results")
} else {
  file.path("R", "04_results")
}

ARCHIVE_DIR <- file.path(RESULTS_DIR, "_archive")
DIAG_DIR    <- file.path(RESULTS_DIR, "diagnostics")

stopifnot(dir.exists(RESULTS_DIR))

cat(strrep("=", 78), "\n")
cat("R/04_results RESTRUCTURE -", if (DRY_RUN) "DRY RUN (dosya degismeyecek)" else "GERCEK CALISTIRMA", "\n")
cat(strrep("=", 78), "\n\n")

# ------------------------------------------------------------------------
# FAZ 1a - ARSIVLENECEK DOSYALAR (yuksek guven: kullanilmadigi/eskimis oldugu
# dogrulandi)
# ------------------------------------------------------------------------
# Gerekce ozeti (onceki analizden):
#   - LHS_PRCC_EN_ref_1-1.R : Drive fullText araması hicbir dosyanin bu betigi
#       kaynak/referans almadigini gosterdi; icerik sabit kodlanmis (hardcoded)
#       ornek veri kullaniyor ve farkli bir dosya adina (fig_prcc_pm1.png)
#       yaziyor - gercek PRCC hattı (01_generate_ssp_outputs.R ->
#       02_figures_cross_scenario.R, fig_prcc.png) bundan bagimsiz. Orphan.
#   - diagnostics/ctmc_spark_diagnostics.R : outputs/model_results/... gibi
#       SSP-oncesi (tek senaryo donemi) eski bir yol yapisi okuyor; guncel
#       3-SSP klasor yapisiyla (outputs/<ssp>/...) uyusmuyor.
#   - diagnostics/diagnostic_analysis_ctmc_spark.R : ayni eskimis yol
#       imzasina sahip (outputs/model_results/...), ayni gerekce.
#   - fig_oat_m_profile_en.Rmd : fig_oat_m_profile_en_tr.Rmd'nin neredeyse
#       birebir alt kumesi (ayni setup chunk'i, sadece Ingilizce figur
#       chunk'i eksik Turkce chunk ile); ikisi de ayni dosyaya
#       (fig_oat_m_profile_en.png) yaziyor -> gereksiz tekrar/duplicate work.
#       "Konsolidasyon" burada = bilingual (_en_tr) versiyonu tut, tekil
#       (_en) versiyonu arsivle.
archive_confirmed <- data.frame(
  file = c(
    "LHS_PRCC_EN_ref_1-1.R",
    "fig_oat_m_profile_en.Rmd",
    file.path("diagnostics", "ctmc_spark_diagnostics.R"),
    file.path("diagnostics", "diagnostic_analysis_ctmc_spark.R")
  ),
  reason = c(
    "Orphan: hicbir betik/Rmd bunu source/referans etmiyor (Drive fullText dogrulandi); sabit kodlanmis ornek veri kullaniyor.",
    "Redundant: fig_oat_m_profile_en_tr.Rmd'nin alt kumesi; ayni PNG'yi (fig_oat_m_profile_en.png) uretiyor.",
    "Eskimis yol yapisi: outputs/model_results/... (SSP-oncesi, tek senaryo donemi) okuyor; guncel outputs/<ssp>/... yapisiyla uyusmuyor.",
    "Eskimis yol yapisi: ayni SSP-oncesi yol imzasi (outputs/model_results/...); guncel yapiyla uyusmuyor."
  ),
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------------------
# FAZ 1b - ONAY BEKLEYEN ADAY (ayri tutuluyor - kullanicinin ek onayi gerekli)
# ------------------------------------------------------------------------
# bulgular_sunum.Rmd vs bulgular_yeni_tam_guncel.Rmd karsilastirmasi:
#   - bulgular_sunum.Rmd  : DIR_OUT <- "outputs/<ssp>/..."   (CANLI/is-turu agac)
#                            veri butunlugu stop() kontrolu YOK
#                            daha az aciklayici Turkce metin
#                            envanter kontrolu farkli alt klasor adlari kullaniyor
#                            (model_results/, sensitivity/ctmc_mc/...)
#   - bulgular_yeni_tam_guncel.Rmd :
#                            DIR_OUT <- outputs/_canonical/...  (DONMUS/canonical agac)
#                            kanonik_aylik_oku() ile ciddi stop() bazli
#                            butunluk kontrolleri (yinelenen satir kontrolu +
#                            612 ay/ilce x SSP kapsam kontrolu)
#                            cok daha fazla aciklayici Turkce metin
#                            envanter kontrolu analytic/, oat/ alt klasor
#                            adlarini kullaniyor
#
#   Yorum: "_guncel" (guncellenmis) dosya adi + canonical agactan okuma +
#   sıkı veri butunlugu kontrolleri, bunun daha SONRA sertlestirilmis/nihai
#   versiyon oldugunu gosteriyor. bulgular_sunum.Rmd byanca "sunum" (=sunum/
#   presentation) adi tasidigindan, kasitli olarak ayri/kisa bir sunum
#   formati olma ihtimali de var - bu yuzden otomatik arsivlemiyorum.
#   ONERI: Eger bulgular_sunum.Rmd ayri bir sunum icin AKTIF olarak
#   kullanilmiyorsa (ör. bir sunumda/toplantida kullanilmadiysa), muhtemelen
#   supersede edilmis bir taslaktir ve asagidaki listeye eklenip
#   arsivlenebilir. Kullanicinin bu tek soruyu yanitlamasi yeterli.
archive_pending_confirmation <- data.frame(
  file = c("bulgular_sunum.Rmd"),
  reason = c("Muhtemelen supersede edilmis taslak (canli agac okuyor, butunluk kontrolu yok, daha az prose) - AMA 'sunum' adi kasitli ayri bir sunum formati olabilecegini dusundurur. Otomatik arsivlenmedi; ekleyip calistirmadan once teyit et."),
  stringsAsFactors = FALSE
)
# Bu dosyayi arsivlemeye DAHIL ETMEK icin: yukaridaki archive_confirmed
# tablosuna elle ekle, ya da asagidaki satiri yorumdan cikar:
# archive_confirmed <- rbind(archive_confirmed, archive_pending_confirmation)

# ------------------------------------------------------------------------
# FAZ 1c - AKTIF/KORUNACAK (degisiklik yok, sadece bilgi amacli listelendi)
# ------------------------------------------------------------------------
keep_active <- c(
  "00_results_setup.R",                 # paylasilan setup - TUM betiklerin bagimliligi
  "01_generate_ssp_outputs.R",
  "02_figures_cross_scenario.R",
  "run_pipeline_3_4.R",
  "OAT_full_analysis.R",
  "10_tau_threshold_sensitivity.R",
  "11_csi_isi_haritasi.R",
  "kvpd_sensitivity_check.R",
  "6_10_pest_regression.R",
  "6_10b_regression_ols_lmm.R",
  "build_eip_sdlog_table.R",
  "convexity_check.R",
  "taylor_proof_and_validation.R",
  "ek_a4_poisson_bernoulli_check.R",
  "mc_validation_vs_analytic.R",
  "fig_decade_progression.R",
  "fig_pest_sensitivity_3ssp.R",
  "fig_oat_m_profile_en_tr.Rmd",
  "bulgular_yeni_tam_guncel.Rmd",
  "m_sensitivity_ek_analizler.Rmd",
  "sicaklik_risk_ek_analizler.Rmd",
  file.path("diagnostics", "r0_histogram.R")  # her iki bulgular Rmd'sinde de kullaniliyor (fig-r0)
)

# ------------------------------------------------------------------------
# FAZ 1d - KAPSAM DISI (04_results icinde degil, sadece bilgi amacli not)
# ------------------------------------------------------------------------
# ek_2_a.Rmd matematiksel bir ek (Appendix A turetmesi), veri isleme betigi
# degil. R/04_results/ icinde durmasi mantikli degil ama Q1'e gore bu betik
# SADECE 04_results icini duzenliyor - bu dosyayi baska bir klasore (orn.
# manuscript/ appendix/) tasimak, bu betigin kapsaminin disinda birakildi.
# Manuel olarak degerlendirilmesi onerilir.
cat("[BILGI] Kapsam disi not: ek_2_a.Rmd matematiksel ek icerigi tasiyor;\n")
cat("        veri/figur uretmiyor. R/04_results disina (orn. manuscript/appendix/)\n")
cat("        tasinmasi mantikli olabilir ama bu, bu betigin kapsami disinda -\n")
cat("        manuel karar/aksiyon gerektirir.\n\n")

# ==========================================================================
# FAZ 2 - ONERILEN ALT KLASOR YAPISI (SADECE PLAN - bu betik TASIMIYOR)
# ==========================================================================
# Asagidaki yapi R/04_results'i daha kompakt/anlasilir hale getirir, ama
# HENUZ UYGULANMIYOR: her betigin source()/here() cagrilari tek tek
# dogrulanmadan dosyalari tasimak calisan pipeline'i kirabilir (ör.
# source("00_results_setup.R") gibi goreli yollar, dosya farkli bir alt
# klasore tasinirsa artik calismaz). Bu, ayri bir "FAZ 2" gecisi olarak
# onerilir: her dosyanin source() satirlari once okunup path'ler
# guncellenmeli, sonra tasima yapilmali.
proposed_structure <- list(
  "(kok - degismez)"    = "00_results_setup.R",
  "core/"               = c("01_generate_ssp_outputs.R", "02_figures_cross_scenario.R", "run_pipeline_3_4.R"),
  "sensitivity/"        = c("kvpd_sensitivity_check.R", "10_tau_threshold_sensitivity.R",
                             "OAT_full_analysis.R", "fig_oat_m_profile_en_tr.Rmd",
                             "fig_pest_sensitivity_3ssp.R"),
  "regression/"         = c("6_10_pest_regression.R", "6_10b_regression_ols_lmm.R", "build_eip_sdlog_table.R"),
  "validation/"         = c("convexity_check.R", "taylor_proof_and_validation.R",
                             "mc_validation_vs_analytic.R", "ek_a4_poisson_bernoulli_check.R"),
  "figures_misc/"       = c("fig_decade_progression.R", "11_csi_isi_haritasi.R"),
  "reports/"            = c("bulgular_yeni_tam_guncel.Rmd", "m_sensitivity_ek_analizler.Rmd",
                             "sicaklik_risk_ek_analizler.Rmd"),
  "diagnostics/ (aynen kalir)" = "r0_histogram.R",
  "_archive/"           = c(archive_confirmed$file)
)

cat(strrep("-", 78), "\n")
cat("FAZ 2 ONERISI (uygulanmadi - sadece plan):\n")
cat(strrep("-", 78), "\n")
for (nm in names(proposed_structure)) {
  cat(sprintf("  %s\n", nm))
  for (f in proposed_structure[[nm]]) cat(sprintf("      - %s\n", f))
}
cat("\n")

# ==========================================================================
# FAZ 1 UYGULAMA - sadece _archive/ tasima (guvenli, source() bagimliligi yok)
# ==========================================================================
cat(strrep("-", 78), "\n")
cat("FAZ 1 PLANI (arsivleme - _archive/ altina TASIMA, SILME DEGIL):\n")
cat(strrep("-", 78), "\n")

plan <- archive_confirmed
plan$from <- file.path(RESULTS_DIR, plan$file)
plan$to   <- file.path(ARCHIVE_DIR, basename(plan$file))
plan$exists <- file.exists(plan$from)

for (i in seq_len(nrow(plan))) {
  status <- if (plan$exists[i]) "BULUNDU" else "!! BULUNAMADI (yol/isim kontrol et) !!"
  cat(sprintf("\n[%d] %s\n", i, plan$file[i]))
  cat(sprintf("    Durum : %s\n", status))
  cat(sprintf("    Gerekce: %s\n", plan$reason[i]))
  cat(sprintf("    %s -> %s\n", plan$from[i], plan$to[i]))
}

cat("\n")
cat(sprintf("Onay bekleyen (otomatik dahil edilmedi): %s\n",
            paste(archive_pending_confirmation$file, collapse = ", ")))
cat(strrep("=", 78), "\n\n")

if (DRY_RUN) {
  cat(">>> DRY_RUN = TRUE: hicbir dosya tasinmadi. Plani incele, uygunsa\n")
  cat(">>> DRY_RUN <- FALSE yapip tekrar calistir.\n")
} else {
  if (!dir.exists(ARCHIVE_DIR)) dir.create(ARCHIVE_DIR, recursive = TRUE)
  ok <- plan$exists
  if (any(!ok)) {
    warning(sprintf("Su dosyalar bulunamadi, atlaniyor: %s",
                     paste(plan$file[!ok], collapse = ", ")))
  }
  for (i in which(ok)) {
    file.rename(plan$from[i], plan$to[i])
    cat(sprintf("TASINDI: %s -> %s\n", plan$file[i], plan$to[i]))
  }
  cat("\nBitti. Geri almak icin dosyalari _archive/ altindan eski konumlarina\n")
  cat("elle tasiyabilirsin (hicbir dosya silinmedi).\n")
}
