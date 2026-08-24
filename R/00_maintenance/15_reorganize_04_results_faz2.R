# ==============================================================================
# 15_reorganize_04_results_faz2.R
#
# AMAC (FAZ 2 - 14_restructure_04_results.R'nin devami):
#   R/04_results/ icindeki betikleri, 14_restructure_04_results.R'de onerilen
#   alt klasor yapisina (core/, sensitivity/, regression/, validation/,
#   figures_misc/, reports/) GERCEKTEN tasir VE bu tasimadan etkilenen TUM
#   source()/here()/file.path() referanslarini otomatik gunceller.
#
#   Bu betik yazilmadan once TUM proje (R/, reports/, R/00_maintenance/)
#   "04_results" ve tek tek her dosya adi icin taranmistir. Bulunan TEK
#   fonksiyonel bagimlilik zinciri:
#     (a) R/run_all.R  - HER 04_results betigini source_utf8("R/04_results/<dosya>")
#         seklinde SABIT KODLANMIS (hardcoded) string ile cagiriyor. Bu,
#         guncellenmesi GEREKEN ana dosya.
#     (b) R/04_results/02_figures_cross_scenario.R -> 01_generate_ssp_outputs.R'i
#         here("R","04_results","01_generate_ssp_outputs.R") ile cagiriyor.
#     (c) R/04_results/run_pipeline_3_4.R -> GEN_FILE/XSCN_FILE degiskenlerinde
#         01_generate_ssp_outputs.R ve 02_figures_cross_scenario.R yollarini
#         file.path("R","04_results",...) ile sabit kodluyor.
#     (d) 00_results_setup.R'i source eden butun dosyalar (01_generate_ssp_outputs.R,
#         6_10_pest_regression.R, fig_oat_m_profile_en_tr.Rmd) bunu KOK'ten
#         (R/04_results/00_results_setup.R) okuyor - 00_results_setup.R
#         TASINMIYOR (kok'te kaliyor), o yuzden bu satirlarda DEGISIKLIK
#         GEREKMIYOR (referans hala gecerli).
#     (e) OAT_full_analysis.R, fig_oat_m_profile_en_tr.Rmd, m_sensitivity_ek_analizler.Rmd,
#         sicaklik_risk_ek_analizler.Rmd, diagnostics/r0_histogram.R, ek_2_a.Rmd:
#         hicbir betik/rapor bunlari source() ETMIYOR - bulgular Rmd'leri
#         bunlarin URETTIGI PNG/CSV CIKTILARINI (sabit outputs/ yollarindan)
#         okuyor, R KODUNU degil. Bu yuzden bu 6 dosya tasindiginda HICBIR
#         referans guncellemesi GEREKMEZ (yalnizca kullanicinin bir dahaki
#         sefere bu betikleri MANUEL calistirirken yeni konumu bilmesi yeterli).
#
#   Ayrica: R/run_all.R "LHS_PRCC_EN_ref_1-1" adimini hala cagiriyor, ama bu
#   dosya 14_restructure_04_results.R ile ONCEKI ADIMDA zaten _archive/'a
#   tasindi (orphan/kullanilmiyor oldugu dogrulanmisti). source_utf8() dosya
#   yoksa stop() ile durur -> run_all.R artik BU ADIMDA HATA VERIR. Bu betik
#   bu adimi run_all.R icinde YORUMA ALIR (silmez) - bagimsiz bir duzeltme,
#   FAZ 2 tasimasindan ayri ama onunla birlikte cozulmesi gerekiyordu.
#
# GUVENLIK MODELI:
#   - DRY_RUN <- TRUE iken: HICBIR dosya tasinmaz, HICBIR metin dosyasi
#     degistirilmez. Sadece planlanan degisiklikler (satir satir "ESKI ->
#     YENI" diff formatinda) konsola yazdirilir.
#   - DRY_RUN <- FALSE oldugunda: (1) degisecek metin dosyalarinin (.bak
#     uzantili) yedegi alinir, (2) referanslar guncellenir, (3) dosyalar
#     yeni alt klasorlere tasinir (file.rename - SILME DEGIL).
#   - Hicbir islem geri alinamaz DEGILDIR: .bak dosyalari ve basit
#     file.rename islemleri manuel olarak kolayca geri cevrilebilir.
#
# KULLANIM:
#   1) DRY_RUN <- TRUE ile once calistir, ciktiyi dikkatlice oku.
#   2) Sonuc dogruysa DRY_RUN <- FALSE yap, tekrar calistir.
#   3) Calistirdiktan sonra: STAGES <- c("04"); source("R/run_all.R") ile
#      (ya da en azindan tek bir SSP icin tek bir adimla) pipeline'in hala
#      dogru calistigini DOGRULA.
# ==============================================================================

DRY_RUN <- TRUE   # <-- once TRUE ile calistir, ciktiyi incele, sonra FALSE yap

PROJ <- tryCatch(
  rprojroot::find_root(rprojroot::has_file("r_project_tez.Rproj")),
  error = function(e) getwd()
)
setwd(PROJ)

RESULTS_DIR <- file.path(PROJ, "R", "04_results")
RUN_ALL     <- file.path(PROJ, "R", "run_all.R")
stopifnot(dir.exists(RESULTS_DIR), file.exists(RUN_ALL))

cat(strrep("=", 78), "\n")
cat("R/04_results FAZ 2 REORGANIZASYON -", if (DRY_RUN) "DRY RUN (hicbir sey degismeyecek)" else "GERCEK CALISTIRMA", "\n")
cat(strrep("=", 78), "\n\n")

# ------------------------------------------------------------------------
# 1) TASIMA PLANI: (04_results'e gore goreli) eski_yol -> yeni_yol
#    NOT: 00_results_setup.R, ek_2_a.Rmd ve diagnostics/r0_histogram.R
#    KASITLI OLARAK bu listede YOK - onlar yerinde kaliyor.
# ------------------------------------------------------------------------
move_plan <- data.frame(
  from = c(
    "01_generate_ssp_outputs.R",
    "02_figures_cross_scenario.R",
    "run_pipeline_3_4.R",
    "kvpd_sensitivity_check.R",
    "10_tau_threshold_sensitivity.R",
    "OAT_full_analysis.R",
    "fig_oat_m_profile_en_tr.Rmd",
    "fig_pest_sensitivity_3ssp.R",
    "6_10_pest_regression.R",
    "6_10b_regression_ols_lmm.R",
    "build_eip_sdlog_table.R",
    "convexity_check.R",
    "taylor_proof_and_validation.R",
    "mc_validation_vs_analytic.R",
    "ek_a4_poisson_bernoulli_check.R",
    "fig_decade_progression.R",
    "11_csi_isi_haritasi.R",
    "m_sensitivity_ek_analizler.Rmd",
    "sicaklik_risk_ek_analizler.Rmd"
  ),
  to = c(
    "core/01_generate_ssp_outputs.R",
    "core/02_figures_cross_scenario.R",
    "core/run_pipeline_3_4.R",
    "sensitivity/kvpd_sensitivity_check.R",
    "sensitivity/10_tau_threshold_sensitivity.R",
    "sensitivity/OAT_full_analysis.R",
    "sensitivity/fig_oat_m_profile_en_tr.Rmd",
    "sensitivity/fig_pest_sensitivity_3ssp.R",
    "regression/6_10_pest_regression.R",
    "regression/6_10b_regression_ols_lmm.R",
    "regression/build_eip_sdlog_table.R",
    "validation/convexity_check.R",
    "validation/taylor_proof_and_validation.R",
    "validation/mc_validation_vs_analytic.R",
    "validation/ek_a4_poisson_bernoulli_check.R",
    "figures_misc/fig_decade_progression.R",
    "figures_misc/11_csi_isi_haritasi.R",
    "reports/m_sensitivity_ek_analizler.Rmd",
    "reports/sicaklik_risk_ek_analizler.Rmd"
  ),
  stringsAsFactors = FALSE
)
move_plan$from_abs <- file.path(RESULTS_DIR, move_plan$from)
move_plan$to_abs   <- file.path(RESULTS_DIR, move_plan$to)
move_plan$exists   <- file.exists(move_plan$from_abs)

cat("[NOT] Yerinde kalanlar (tasinmiyor): 00_results_setup.R, ek_2_a.Rmd,\n")
cat("      diagnostics/r0_histogram.R\n\n")

cat(strrep("-", 78), "\n")
cat("TASIMA PLANI:\n")
cat(strrep("-", 78), "\n")
for (i in seq_len(nrow(move_plan))) {
  status <- if (move_plan$exists[i]) "BULUNDU" else "!! BULUNAMADI !!"
  cat(sprintf("[%2d] %-45s -> %-45s [%s]\n",
              i, move_plan$from[i], move_plan$to[i], status))
}
cat("\n")

if (any(!move_plan$exists)) {
  warning("Bazi kaynak dosyalar bulunamadi - devam etmeden once yollari kontrol edin:\n  ",
          paste(move_plan$from[!move_plan$exists], collapse = "\n  "))
}

# ------------------------------------------------------------------------
# 2) METIN REFERANS GUNCELLEMELERI
#    Her giris: (duzenlenecek dosya, TAM/SABIT eski string, yeni string)
#    fixed = TRUE kullanilir (regex degil, duz metin eslesmesi).
# ------------------------------------------------------------------------
text_edits <- list(
  # --- R/run_all.R : ana orkestrator, TUM 04_results cagrilari burada ---
  list(file = RUN_ALL,
       old  = 'source_utf8("R/04_results/run_pipeline_3_4.R")',
       new  = 'source_utf8("R/04_results/core/run_pipeline_3_4.R")'),
  list(file = RUN_ALL,
       old  = 'source_utf8("R/04_results/01_generate_ssp_outputs.R")',
       new  = 'source_utf8("R/04_results/core/01_generate_ssp_outputs.R")'),
  list(file = RUN_ALL,
       old  = 'source_utf8("R/04_results/02_figures_cross_scenario.R")',
       new  = 'source_utf8("R/04_results/core/02_figures_cross_scenario.R")'),
  list(file = RUN_ALL,
       old  = 'source_utf8("R/04_results/mc_validation_vs_analytic.R")',
       new  = 'source_utf8("R/04_results/validation/mc_validation_vs_analytic.R")'),
  list(file = RUN_ALL,
       old  = 'source_utf8("R/04_results/11_csi_isi_haritasi.R")',
       new  = 'source_utf8("R/04_results/figures_misc/11_csi_isi_haritasi.R")'),
  list(file = RUN_ALL,
       old  = 'source_utf8("R/04_results/6_10_pest_regression.R")',
       new  = 'source_utf8("R/04_results/regression/6_10_pest_regression.R")'),
  list(file = RUN_ALL,
       old  = 'source_utf8("R/04_results/6_10b_regression_ols_lmm.R")',
       new  = 'source_utf8("R/04_results/regression/6_10b_regression_ols_lmm.R")'),
  list(file = RUN_ALL,
       old  = 'source_utf8("R/04_results/taylor_proof_and_validation.R")',
       new  = 'source_utf8("R/04_results/validation/taylor_proof_and_validation.R")'),
  list(file = RUN_ALL,
       old  = 'source_utf8("R/04_results/convexity_check.R")',
       new  = 'source_utf8("R/04_results/validation/convexity_check.R")'),
  list(file = RUN_ALL,
       old  = 'source_utf8("R/04_results/build_eip_sdlog_table.R")',
       new  = 'source_utf8("R/04_results/regression/build_eip_sdlog_table.R")'),
  list(file = RUN_ALL,
       old  = 'source_utf8("R/04_results/10_tau_threshold_sensitivity.R")',
       new  = 'source_utf8("R/04_results/sensitivity/10_tau_threshold_sensitivity.R")'),
  list(file = RUN_ALL,
       old  = 'source_utf8("R/04_results/kvpd_sensitivity_check.R")',
       new  = 'source_utf8("R/04_results/sensitivity/kvpd_sensitivity_check.R")'),
  list(file = RUN_ALL,
       old  = 'source_utf8("R/04_results/fig_decade_progression.R")',
       new  = 'source_utf8("R/04_results/figures_misc/fig_decade_progression.R")'),
  list(file = RUN_ALL,
       old  = 'source_utf8("R/04_results/fig_pest_sensitivity_3ssp.R")',
       new  = 'source_utf8("R/04_results/sensitivity/fig_pest_sensitivity_3ssp.R")'),
  # EK_A4 uc-yollu kosul: v2-adli varyant (henuz yok ama kod bunu once ariyor)
  # + kok'teki eski kopya (yok) + gercek varsayilan (04_results kokunde, v2 icerigi)
  list(file = RUN_ALL,
       old  = '"R/04_results/ek_a4_poisson_bernoulli_check_v2.R"',
       new  = '"R/04_results/validation/ek_a4_poisson_bernoulli_check_v2.R"'),
  list(file = RUN_ALL,
       old  = '"R/04_results/ek_a4_poisson_bernoulli_check.R"',
       new  = '"R/04_results/validation/ek_a4_poisson_bernoulli_check.R"'),

  # --- R/04_results/02_figures_cross_scenario.R (kendisi core/'a tasiniyor) ---
  list(file = file.path(RESULTS_DIR, "02_figures_cross_scenario.R"),
       old  = 'here("R", "04_results", "01_generate_ssp_outputs.R")',
       new  = 'here("R", "04_results", "core", "01_generate_ssp_outputs.R")'),

  # --- R/04_results/run_pipeline_3_4.R (kendisi core/'a tasiniyor) ---
  list(file = file.path(RESULTS_DIR, "run_pipeline_3_4.R"),
       old  = 'file.path("R", "04_results", "01_generate_ssp_outputs.R")',
       new  = 'file.path("R", "04_results", "core", "01_generate_ssp_outputs.R")'),
  list(file = file.path(RESULTS_DIR, "run_pipeline_3_4.R"),
       old  = 'file.path("R", "04_results", "02_figures_cross_scenario.R")',
       new  = 'file.path("R", "04_results", "core", "02_figures_cross_scenario.R")')
)

# ------------------------------------------------------------------------
# 3) ORPHAN ADIM DUZELTMESI: LHS_PRCC_EN_ref_1-1 zaten _archive/'da.
#    run_all.R'deki cagriyi SIL(mez), YORUMA ALIR.
# ------------------------------------------------------------------------
orphan_line_pattern <- 'step("LHS_PRCC_EN_ref_1-1",        source_utf8("R/04_results/LHS_PRCC_EN_ref_1-1.R"))'
orphan_replacement  <- paste0(
  '# [ARSIVLENDI 2026-08-22: R/04_results/_archive/LHS_PRCC_EN_ref_1-1.R - orphan/kullanilmiyor, bkz. 14_restructure_04_results.R] ',
  orphan_line_pattern
)

# ==========================================================================
# ONIZLEME: her metin dosyasi icin ESKI -> YENI satir satir goster
# ==========================================================================
cat(strrep("-", 78), "\n")
cat("METIN REFERANS GUNCELLEME PLANI:\n")
cat(strrep("-", 78), "\n")

files_to_edit <- unique(vapply(text_edits, function(x) x$file, character(1)))
files_to_edit <- union(files_to_edit, RUN_ALL)  # orphan-fix de RUN_ALL'da

new_contents <- list()  # dosya -> guncellenmis satirlar (yaziimaya hazir)

for (fp in files_to_edit) {
  if (!file.exists(fp)) {
    cat(sprintf("\n!! Dosya bulunamadi, atlaniyor: %s\n", fp))
    next
  }
  lines <- readLines(fp, warn = FALSE, encoding = "UTF-8")
  cat(sprintf("\n--- %s ---\n", sub(paste0("^", PROJ, "/?"), "", fp, fixed = FALSE)))

  edits_here <- Filter(function(x) x$file == fp, text_edits)
  for (e in edits_here) {
    hits <- grep(e$old, lines, fixed = TRUE)
    if (length(hits) == 0) {
      cat(sprintf("  [UYARI] Bulunamadi (zaten degismis olabilir): %s\n", e$old))
      next
    }
    for (h in hits) {
      cat(sprintf("  satir %d:\n    - %s\n    + %s\n",
                   h, lines[h], gsub(e$old, e$new, lines[h], fixed = TRUE)))
      lines[h] <- gsub(e$old, e$new, lines[h], fixed = TRUE)
    }
  }

  # orphan-adim yorum satirina cevirme (yalnizca RUN_ALL icin anlamli)
  if (identical(fp, RUN_ALL)) {
    hits2 <- grep(orphan_line_pattern, lines, fixed = TRUE)
    for (h in hits2) {
      cat(sprintf("  satir %d (orphan adim yoruma alindi):\n    - %s\n    + %s\n",
                   h, lines[h], orphan_replacement))
      lines[h] <- orphan_replacement
    }
  }

  new_contents[[fp]] <- lines
}
cat("\n")

# ==========================================================================
# UYGULAMA
# ==========================================================================
if (DRY_RUN) {
  cat(strrep("=", 78), "\n")
  cat(">>> DRY_RUN = TRUE: hicbir dosya tasinmadi, hicbir metin degismedi.\n")
  cat(">>> Plani incele; uygunsa DRY_RUN <- FALSE yapip tekrar calistir.\n")
  cat(strrep("=", 78), "\n")
} else {
  cat(strrep("=", 78), "\n")
  cat("UYGULANIYOR...\n")
  cat(strrep("=", 78), "\n\n")

  # --- (a) metin dosyalarini .bak yedekleyip yaz ---
  for (fp in names(new_contents)) {
    bak <- paste0(fp, ".bak_faz2_", format(Sys.Date(), "%Y%m%d"))
    if (!file.exists(bak)) file.copy(fp, bak)
    writeLines(new_contents[[fp]], fp, useBytes = TRUE)
    cat(sprintf("GUNCELLENDI: %s  (yedek: %s)\n",
                sub(paste0("^", PROJ, "/?"), "", fp), basename(bak)))
  }

  # --- (b) hedef alt klasorleri olustur ---
  target_dirs <- unique(dirname(move_plan$to_abs))
  for (d in target_dirs) if (!dir.exists(d)) dir.create(d, recursive = TRUE)

  # --- (c) dosyalari tasi ---
  cat("\n")
  ok <- move_plan$exists
  for (i in which(ok)) {
    file.rename(move_plan$from_abs[i], move_plan$to_abs[i])
    cat(sprintf("TASINDI: %s -> %s\n", move_plan$from[i], move_plan$to[i]))
  }
  if (any(!ok)) {
    warning("Tasinamayan (bulunamayan) dosyalar: ",
            paste(move_plan$from[!ok], collapse = ", "))
  }

  cat("\n>>> Bitti. DOGRULAMA icin oneri:\n")
  cat('    STAGES <- c("04"); source("R/run_all.R", encoding = "UTF-8")\n')
  cat("    (ya da tek bir adimi elle source() ederek hizli kontrol edin)\n")
  cat(">>> Bir sorun cikarsa: .bak_faz2_* dosyalarini eski adlarina geri\n")
  cat(">>> cevirin ve tasinan dosyalari file.rename ile eski konumlarina\n")
  cat(">>> geri alin (hicbir icerik silinmedi).\n")
}
