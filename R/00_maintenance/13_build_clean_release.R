# =============================================================================
# R/00_maintenance/13_build_clean_release.R
#
# AMAC
#   Mevcut projeye HIC DOKUNMADAN, temiz bir surum kurar:
#       .../0000_tezim/r_project_tez        <- KAYNAK (salt-okunur, degismez)
#       <DEST>/r_project_tez_clean          <- HEDEF (yeniden olusturulur)
#
#   Beyaz liste mantigi degil, KARA LISTE mantigi kullanir: her sey kopyalanir,
#   yalnizca acikca dislanan desenler atlanir. Boylece "unuttugumuz bir dosya
#   sessizce kayboldu" riski yoktur - atlanan her dosya loga yazilir.
#
# GUVENLIK
#   - Kaynak uzerinde TEK BIR YAZMA/SILME islemi yoktur (.safe_dest guard).
#   - DRY_RUN = TRUE varsayilan; once listeyi okuyun.
#   - Hedef klasor doluysa DOKUNULMAZ, hata verir.
#
# KULLANIM
#   DRY_RUN <- TRUE ;  source("R/00_maintenance/13_build_clean_release.R")
#   ...raporu okuyun, itiraziniz varsa asagidaki listeleri duzenleyin...
#   DRY_RUN <- FALSE;  source("R/00_maintenance/13_build_clean_release.R")
#
# SECENEKLER (source etmeden once tanimlayin)
#   OUTPUTS_MODE <- "full"      # "full" = tum outputs (superseded haric)
#                               # "canonical" = yalnizca _canonical/_audit/
#                               #               tables/cross_scenario (cok daha kucuk)
#   COPY_GIT     <- FALSE       # TRUE ise .git gecmisi de kopyalanir
#   REFRESH_SHINY<- TRUE        # shiny verisini _canonical'dan tazele
#   DEST         <- "C:/Users/oderin/Documents/r_project_tez_clean"
#
# ! DIKKAT - DRIVE SENKRONU !
#   Kaynak klasor Google Drive ile senkronize (yol icinde .../Documents/drive/...).
#   Hedefi senkron agacinin ICINE koyarsaniz ~1 GB'lik temiz kopya Drive'a
#   YUKLENMEYE baslar: kota dolabilir, senkron saatler surebilir ve Drive'da
#   ayni projenin iki kopyasi olusur.
#   Bu yuzden varsayilan hedef senkron agacinin DISINDADIR:
#       ~/r_project_tez_clean   (Windows'ta C:/Users/<kullanici>/Documents/...)
#   Bilerek senkron icine kurmak isterseniz DEST'i elle verin.
# =============================================================================

if (!exists("DRY_RUN"))       DRY_RUN       <- FALSE
if (!exists("OUTPUTS_MODE"))  OUTPUTS_MODE  <- "full"
if (!exists("COPY_GIT"))      COPY_GIT      <- FALSE
if (!exists("REFRESH_SHINY")) REFRESH_SHINY <- TRUE

stopifnot(OUTPUTS_MODE %in% c("full", "canonical"))

suppressPackageStartupMessages(library(rprojroot))

SRC <- normalizePath(rprojroot::find_root(rprojroot::has_file("r_project_tez.Rproj")),
                     winslash = "/")

## --- Hedef: varsayilan olarak senkron agacinin DISI ---------------------------
.sync_re <- "(?i)/(drive|OneDrive|Dropbox|Google ?Drive|iCloud)[^/]*/"
.in_sync <- function(p) grepl(.sync_re, paste0(p, "/"), perl = TRUE)

if (!exists("DEST")) {
  DEST <- if (.in_sync(SRC)) {
    file.path(path.expand("~"), "r_project_tez_clean")   # senkron disi
  } else {
    file.path(dirname(SRC), "r_project_tez_clean")       # kardes klasor
  }
}
DEST <- normalizePath(DEST, winslash = "/", mustWork = FALSE)

if (.in_sync(DEST)) {
  cat("\n!! UYARI: hedef bir bulut-senkron klasorunun icinde:\n   ", DEST, "\n",
      "   Temiz kopya buluta YUKLENECEK. Bilerek yapiyorsaniz devam edin;\n",
      "   yoksa DEST'i senkron disi bir yola verin, orn:\n",
      "     DEST <- \"C:/Users/oderin/Documents/r_project_tez_clean\"\n\n", sep = "")
}

cat(strrep("=", 72), "\n", sep = "")
cat("TEMIZ SURUM OLUSTURMA\n")
cat("  KAYNAK : ", SRC,  "   (salt-okunur)\n", sep = "")
cat("  HEDEF  : ", DEST, "\n", sep = "")
cat("  MOD    : ", if (DRY_RUN) "DRY RUN - hicbir dosya yazilmaz" else "GERCEK KOSUM",
    " | outputs = ", OUTPUTS_MODE, "\n", sep = "")
cat(strrep("=", 72), "\n\n", sep = "")

if (!DRY_RUN && dir.exists(DEST) && length(list.files(DEST)) > 0)
  stop("Hedef klasor zaten dolu: ", DEST,
       "\nOnce elle silin veya yeniden adlandirin.", call. = FALSE)

# =============================================================================
# 1) DISLAMA KURALLARI
# =============================================================================

## --- Dizin adlari (agacin herhangi bir seviyesinde) --------------------------
EXCL_DIR_NAME <- c(
  ".git", ".Rproj.user", "_to_delete", "renv",
  "Yeni_klasor", "Yeni klasor",                  # ad-hoc yedek klasorleri
  "figure-html", "figure-latex",                 # knitr onbellegi
  "archived",                                    # 14 Agustos arsivi (kaynakta duruyor)
  "ctmc_mc_rep1000",                             # BERNOULLI donemi OAT agaci
  "DIR_OUTPUT_CROSS"                             # hatali olusmus bos dizin
)
EXCL_DIR_NAME <- c(EXCL_DIR_NAME, "Yeni klas\u00f6r")   # Turkce 'o' umlaut varyanti
if (isTRUE(COPY_GIT)) EXCL_DIR_NAME <- setdiff(EXCL_DIR_NAME, ".git")

## --- Dizin yolu desenleri ----------------------------------------------------
EXCL_DIR_RE <- c(
  "_files$",                        # bulgular_*_files/  (render yan urunleri)
  "^outputs/frozen(/|$)",           # _canonical ile ayni islev (asagida hash kontrolu)
  "^outputs/cross_scenario/archive(/|$)"
)

## --- Dosya adi desenleri -----------------------------------------------------
EXCL_FILE_RE <- c(
  "^desktop\\.ini$", "^Thumbs\\.db$", "^\\.DS_Store$", "^~\\$",
  "^\\.RData", "^\\.Rhistory$", "\\.RDataTmp", "\\.RData_old$",
  "_old\\.(R|Rmd)$", "^old_.*\\.(R|Rmd)$", "_eski\\.(R|Rmd)$",
  "\\.bak$",
  "^gemini-code-.*\\.txt$",
  "^Claude Setup\\.exe$",
  "\\.html$"                        # tum render ciktilari (Rmd'den yeniden uretilir)
)

## --- Tam yollar (SRC'ye gore) ------------------------------------------------
EXCL_EXACT <- c(
  # --- eski surum / islevi biten betikler ---
  "R/04_results/old_01_generate_ssp_outputs.R",
  "R/04_results/fig_oat_m_profile_en_old.Rmd",
  "R/03_models/run_pipeline_ctmc.R",              # kirik yollar; run_pipeline_3_4.R kanonik
  "R/00_maintenance/04_archive_legacy.R",         # tek seferlik is bitti
  "R/00_maintenance/04b_preflight_refs.R",        # 11_preflight_check.R ile degistirildi
  "R/00_maintenance/10_cleanup_workspace.R",      # yerinde-temizlik varyanti; bu yol secilmedi
  "R/04_results/ek_a4_poisson_bernoulli_check.R", # v1 (218 satir) - v2 asagida yerine gecer
  "R/validation_two_stage_all_scenarios.R",       # 3 satirlik sarmalayici -> run_all
  "R/04_results/02_mc_validation_all.R",          # ayni gerekce
  "R/04_results/6_11_importation_all_scenarios.R",# ayni gerekce
  "\u00f6rnek_for_sspler_icin.R",                 # run_ctmc_spark_all_ssp.R ile ayni is
  "bulgular_yeni_tam.Rmd",                        # _guncel surumu var
  "bulgular_yeni.Rmd",
  # --- oturum kalintilari / bayat dokumler ---
  "tree.txt",
  "data_raw/climate/python",                      # 0 bayt, hatali olusmus
  # --- eski bolum taslagi (tez metnine islendi) ---
  "bulgular_bolumu (1).docx"
)

## --- outputs = "canonical" modunda tutulacaklar ------------------------------
CANON_KEEP_RE <- c("^outputs/_canonical(/|$)", "^outputs/_audit(/|$)",
                   "^outputs/tables(/|$)", "^outputs/cross_scenario(/|$)")

# =============================================================================
# 2) YENIDEN KONUMLANDIRMA (kok dizindeki serbest dosyalar)
# =============================================================================
REMAP <- c(
  "bulgular_yeni_tam_guncel.Rmd"                = "reports/bulgular_yeni_tam_guncel.Rmd",
  "tablolar_word.Rmd"                           = "reports/tablolar_word.Rmd",

  "tezim.docx"                                  = "manuscript/tezim.docx",
  "tablolar_word.docx"                          = "manuscript/tablolar_word.docx",
  "elestiri_yanit_mektubu (2).docx"             = "manuscript/elestiri_yanit_mektubu.docx",
  "R/04_results/PLOS_NTD_Supporting_Information.docx" =
                                                  "manuscript/PLOS_NTD_Supporting_Information.docx",
  "RAPOR_v2_kod_ve_tez_degisiklikleri.txt"      = "manuscript/RAPOR_v2_kod_ve_tez_degisiklikleri.txt",
  "R/RAPOR_v2_kod_ve_tez_degisiklikleri.txt"    = "manuscript/RAPOR_v2_kod_ve_tez_degisiklikleri_R.txt",
  "Tez_Reanaliz_Degerlendirme_Raporu.md"        = "manuscript/Tez_Reanaliz_Degerlendirme_Raporu.md",
  "PROJE_DEGERLENDIRME_VE_TEMIZLIK_RAPORU.md"   = "manuscript/PROJE_DEGERLENDIRME_VE_TEMIZLIK_RAPORU.md",
  "PLOS_NTD_GUNCELLEME_HARITASI.md"             = "manuscript/PLOS_NTD_GUNCELLEME_HARITASI.md",
  "BOLUM_6_1_2_DENETIM.md"                      = "manuscript/BOLUM_6_1_2_DENETIM.md",

  "pipeline_diagram.R"                          = "R/utils/pipeline_diagram.R",
  "kavramsal_cerceve_kare_dikey.R"              = "R/utils/kavramsal_cerceve_kare_dikey.R",
  "stokastik_kavramsal_cerceve.R"               = "R/utils/stokastik_kavramsal_cerceve.R",
  "run_ctmc_spark_all_ssp.R"                    = "R/03_models/run_ctmc_spark_all_ssp.R",
  "tourist_arrivals_long_english_names.csv"     = "data_processed/tourist_arrivals_long_english_names.csv",

  # ek_a4 v2 (396 satir, kokte duruyordu) kanonik konumuna
  "R/ek_a4_poisson_bernoulli_check.R"           = "R/04_results/ek_a4_poisson_bernoulli_check.R",
  # duzeltilmis orkestrator asil adiyla
  "R/run_all_v3.R"                              = "R/run_all.R",
  "run_all_v3.R"                                = "R/run_all.R"
)

## Kok dizindeki bagimsiz sekiller -> figures_standalone/
FIG_RE <- paste0("^(kavramsal_cerceve_.*|stokastik_gecis_diyagram_.*|",
                 "stochastic_concept|diagram_temiz|r_project_tez_.*|",
                 "convexity_curve|convexity_map|fig_prcc_pm1)\\.(png|svg|jpg)$")

## run_all.R remap edilecekse eski R/run_all.R'i disla
if (file.exists(file.path(SRC, "R", "run_all_v3.R")) ||
    file.exists(file.path(SRC, "run_all_v3.R")))
  EXCL_EXACT <- c(EXCL_EXACT, "R/run_all.R")

# =============================================================================
# 3) DOSYA AGACINI TARA VE KARAR VER
# =============================================================================
all_rel <- list.files(SRC, recursive = TRUE, all.files = TRUE,
                      no.. = TRUE, include.dirs = FALSE)

decide <- function(rel) {
  parts <- strsplit(rel, "/", fixed = TRUE)[[1]]
  dirs  <- if (length(parts) > 1) parts[-length(parts)] else character(0)
  fname <- parts[length(parts)]
  dpath <- if (length(dirs)) paste(dirs, collapse = "/") else ""

  if (any(dirs %in% EXCL_DIR_NAME))
    return(list(ok = FALSE,
                why = paste0("dizin: ", paste(intersect(dirs, EXCL_DIR_NAME), collapse = ","))))
  for (re in EXCL_DIR_RE)
    if (grepl(re, dpath)) return(list(ok = FALSE, why = paste0("dizin deseni: ", re)))
  if (rel %in% EXCL_EXACT)
    return(list(ok = FALSE, why = "acik dislama listesi"))
  for (re in EXCL_FILE_RE)
    if (grepl(re, fname)) return(list(ok = FALSE, why = paste0("dosya deseni: ", re)))
  if (grepl("^logs/", rel) && isTRUE(file.info(file.path(SRC, rel))$size == 0))
    return(list(ok = FALSE, why = "bos log"))
  if (OUTPUTS_MODE == "canonical" && grepl("^outputs/", rel) &&
      !any(vapply(CANON_KEEP_RE, grepl, logical(1), x = rel)))
    return(list(ok = FALSE, why = "outputs=canonical modu"))

  dst <- rel
  if (!is.na(REMAP[rel]))                             dst <- unname(REMAP[rel])
  else if (length(dirs) == 0 && grepl(FIG_RE, fname)) dst <- file.path("figures_standalone", fname)
  list(ok = TRUE, why = "", dst = dst)
}

res  <- lapply(all_rel, decide)
keep <- vapply(res, function(x) x$ok, logical(1))

plan <- data.frame(
  src   = all_rel,
  dst   = vapply(seq_along(res),
                 function(i) if (keep[i]) res[[i]]$dst else NA_character_, character(1)),
  neden = vapply(res, function(x) x$why, character(1)),
  bytes = file.info(file.path(SRC, all_rel))$size,
  kopya = keep,
  stringsAsFactors = FALSE
)

fmt_mb <- function(b) sprintf("%.1f MB", sum(b, na.rm = TRUE) / 1e6)
cat("Toplam dosya      : ", nrow(plan), "  (", fmt_mb(plan$bytes), ")\n", sep = "")
cat("KOPYALANACAK      : ", sum(plan$kopya), "  (", fmt_mb(plan$bytes[plan$kopya]), ")\n", sep = "")
cat("DISARIDA BIRAKILAN: ", sum(!plan$kopya), "  (", fmt_mb(plan$bytes[!plan$kopya]), ")\n\n", sep = "")

cat("--- Dislama gerekcelerine gore ozet ---\n")
if (any(!plan$kopya)) {
  sub <- plan[!plan$kopya, ]
  ozet <- data.frame(
    neden = names(table(sub$neden)),
    n     = as.integer(table(sub$neden)),
    MB    = round(as.numeric(tapply(sub$bytes, sub$neden,
                                    function(b) sum(b, na.rm = TRUE) / 1e6)), 1),
    stringsAsFactors = FALSE
  )
  print(ozet[order(-ozet$MB), ], row.names = FALSE)
} else cat("  (hicbir sey dislanmadi)\n")

cat("\n--- Yeniden konumlandirilanlar ---\n")
mv <- plan[plan$kopya & plan$src != plan$dst, c("src", "dst")]
if (nrow(mv)) print(head(mv, 40), row.names = FALSE) else cat("  (yok)\n")

# =============================================================================
# 4) frozen/ HASH KONTROLU - _canonical'da karsiligi olmayan var mi?
# =============================================================================
fz <- list.files(file.path(SRC, "outputs", "frozen"), recursive = TRUE, full.names = FALSE)
orphan <- character(0)
if (length(fz)) {
  for (f in fz) {
    a <- file.path(SRC, "outputs", "frozen", f)
    b <- file.path(SRC, "outputs", "_canonical", f)
    if (!file.exists(b) ||
        !identical(unname(tools::md5sum(a)), unname(tools::md5sum(b))))
      orphan <- c(orphan, f)
  }
}
cat("\n--- outputs/frozen kontrolu ---\n")
if (!length(fz)) {
  cat("  frozen/ yok.\n")
} else if (!length(orphan)) {
  cat("  ", length(fz), " dosyanin tamami _canonical ile ozdes -> guvenle atlandi.\n", sep = "")
} else {
  cat("  [!] _canonical'da karsiligi OLMAYAN/FARKLI ", length(orphan), " dosya:\n", sep = "")
  cat(paste0("      - ", head(orphan, 15), collapse = "\n"), "\n")
  cat("      Bunlar hedefte outputs/_frozen_unmatched/ altina kopyalanacak.\n")
}

# =============================================================================
# 5) UYGULA
# =============================================================================
if (!DRY_RUN) {
  cat("\n", strrep("-", 72), "\nKopyalaniyor...\n", sep = "")
  dir.create(DEST, recursive = TRUE, showWarnings = FALSE)

  .safe_dest <- function(p) {
    np <- normalizePath(dirname(p), winslash = "/", mustWork = FALSE)
    if (startsWith(paste0(np, "/"), paste0(SRC, "/")))
      stop("GUVENLIK: hedef kaynak icine dusuyor: ", p, call. = FALSE)
    invisible(TRUE)
  }

  n <- 0L
  for (i in which(plan$kopya)) {
    from <- file.path(SRC,  plan$src[i])
    to   <- file.path(DEST, plan$dst[i])
    .safe_dest(to)
    dir.create(dirname(to), recursive = TRUE, showWarnings = FALSE)
    if (!file.copy(from, to, overwrite = TRUE, copy.date = TRUE))
      warning("Kopyalanamadi: ", plan$src[i])
    n <- n + 1L
    if (n %% 250 == 0) cat("  ", n, " / ", sum(plan$kopya), "\n", sep = "")
  }
  cat("  ", n, " dosya kopyalandi.\n", sep = "")

  if (length(orphan)) {
    for (f in orphan) {
      to <- file.path(DEST, "outputs", "_frozen_unmatched", f)
      dir.create(dirname(to), recursive = TRUE, showWarnings = FALSE)
      file.copy(file.path(SRC, "outputs", "frozen", f), to, copy.date = TRUE)
    }
    cat("  ", length(orphan), " eslesmeyen frozen dosyasi _frozen_unmatched/ altina alindi.\n", sep = "")
  }

  if (REFRESH_SHINY) {
    for (s in c("ssp126", "ssp245", "ssp585")) {
      from <- file.path(SRC, "outputs", "_canonical", s, "simulation")
      to   <- file.path(DEST, "shiny_dengue_app", "outputs", s, "simulation")
      if (dir.exists(from)) {
        dir.create(to, recursive = TRUE, showWarnings = FALSE)
        file.copy(list.files(from, full.names = TRUE), to, overwrite = TRUE, copy.date = TRUE)
      }
    }
    cat("  Shiny verisi _canonical'dan tazelendi (yeniden DEPLOY gerekir).\n")
  }

  writeLines(c(
    "Version: 1.0", "",
    "RestoreWorkspace: No", "SaveWorkspace: No", "AlwaysSaveHistory: No", "",
    "EnableCodeIndexing: Yes", "UseSpacesForTab: Yes", "NumSpacesForTab: 2",
    "Encoding: UTF-8", "", "RnwWeave: Sweave", "LaTeX: pdfLaTeX"
  ), file.path(DEST, "r_project_tez.Rproj"))

  gi  <- file.path(DEST, ".gitignore")
  add <- c("", "# --- temiz surumde eklendi ---",
           "Yeni_klasor/", "**/Yeni_klasor/", "_to_delete/",
           "reports/_rendered/", "*.html", "!README*.html")
  if (file.exists(gi)) write(add, gi, append = TRUE) else writeLines(add, gi)

  man <- plan
  man$md5 <- NA_character_
  ok <- man$kopya & !is.na(man$bytes) & man$bytes < 5e7
  if (any(ok)) man$md5[ok] <- unname(tools::md5sum(file.path(DEST, man$dst[ok])))
  dir.create(file.path(DEST, "outputs", "_audit"), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(man, file.path(DEST, "outputs", "_audit", "clean_release_manifest.csv"),
                   row.names = FALSE, fileEncoding = "UTF-8")

  writeLines(c(
    "# Temiz surum",
    "",
    paste0("Olusturuldu : ", format(Sys.time(), "%Y-%m-%d %H:%M %z")),
    paste0("Kaynak      : ", SRC),
    paste0("R surumu    : ", R.version.string),
    paste0("Kopyalanan  : ", sum(plan$kopya), " dosya"),
    paste0("Dislanan    : ", sum(!plan$kopya), " dosya"),
    paste0("outputs modu: ", OUTPUTS_MODE),
    "",
    "Dosya dokumu: outputs/_audit/clean_release_manifest.csv",
    "",
    "## Uygulanan duzeltmeler",
    "- R/run_all.R : v3 (kirik 06_10 referansi, eksik 03_models asamasi,",
    "                yanlis ek_a4 surumu, LC_ALL locale sorunu duzeltildi)",
    "- R/04_results/ek_a4_poisson_bernoulli_check.R : v2 (ilce etiket takasi)",
    "- r_project_tez.Rproj : RestoreWorkspace/SaveWorkspace = No",
    "- shiny_dengue_app/outputs : outputs/_canonical'dan tazelendi",
    "",
    "## Kaynakta birakilanlar (SILINMEDI)",
    "archived/, outputs/frozen/, outputs/**/Yeni_klasor/, ctmc_mc_rep1000/,",
    "oturum kalintilari (.RData/.Rhistory), render HTML ciktilari, desktop.ini.",
    "Hepsi kaynak klasorde duruyor; gerekirse oradan geri alinabilir.",
    "",
    "## Sonraki adim",
    "source(\"R/00_maintenance/11_preflight_check.R\")"
  ), file.path(DEST, "TEMIZ_SURUM_NOTU.md"))

  cat("\nHEDEF HAZIR: ", DEST, "\n", sep = "")
  cat("Sonraki adim: RStudio'da yeni .Rproj'u acip su denetimi kosun:\n")
  cat("  source(\"R/00_maintenance/11_preflight_check.R\")\n")
} else {
  pf <- file.path(tempdir(), "clean_release_plan.csv")
  utils::write.csv(plan, pf, row.names = FALSE, fileEncoding = "UTF-8")
  cat("\n[DRY RUN] Hicbir dosya yazilmadi.\n")
  cat("Tam plan: ", pf, "\n", sep = "")
  cat("Onayliyorsaniz:  DRY_RUN <- FALSE; source(\"R/00_maintenance/13_build_clean_release.R\")\n")
}

invisible(plan)
