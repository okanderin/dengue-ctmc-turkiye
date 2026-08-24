# =============================================================================
# 09_audit_district_labels.R                                    [SURUM 2]
# -----------------------------------------------------------------------------
# AMAC
#   Tum betiklerde district_id -> ilce adi eslemesini denetler.
#   build_eip_sdlog_table.R'da uc ilcenin dongusel kaymis olmasi
#   (Kartal -> Egirdir -> Fethiye -> Kartal) bu denetimle bulundu; tekrarini
#   onler. Ayrica uretilmis tablolarda tur atamasini ve ilce basina ay
#   sayisini dogrular.
#
# SURUM 2 DUZELTMESI
#   Surum 1 satir icinde konum hesabi (regexpr + substr) kullaniyordu ve
#   ham metin parcalarini ilce adi saniyordu (30 sahte uyari). Bu surum
#   id ve ad'i TEK regex ile CIFT olarak yakalar; sahte uyari uretmez.
#   Takas: farkli satirlara yayilmis tanimlar denetlenmez.
#
# KOSUM
#   source(here::here("R","00_maintenance","09_audit_district_labels.R"))
# =============================================================================

library(here)

# --- Dogru esleme (deger = regex; Turkce karakter varyantlari icin) ----------
DOGRU <- c(
  "TUR.40.25_1" = "Kartal",
  "TUR.59.4_1"  = "Fethiye",
  "TUR.10.4_1"  = "Hopa",
  "TUR.81.6_1"  = "Zonguldak",
  "TUR.39.3_1"  = "E[gğ]irdir"
)

ILCE_RE <- "Kartal|Fethiye|Hopa|Zonguldak|E[gğ]irdir"

# id ve ad'i tek yakalamada eslestiren desen:
#   "TUR.59.4_1" = "Fethiye (Mugla)"
#   TUR.59.4_1 = "Fethiye"
#   "TUR.59.4_1",   "Fethiye",
CIFT_RE <- paste0('(TUR\\.[0-9]+\\.[0-9]+_1)"?\\s*[,=]\\s*"?\\s*(', ILCE_RE, ')')

# --- Taranacak dosyalar ------------------------------------------------------
fs <- c(
  list.files(here("R"), "\\.(R|Rmd)$", recursive = TRUE, full.names = TRUE),
  list.files(here(),    "\\.(R|Rmd)$", full.names = TRUE),
  here("shiny_dengue_app", "app.R")
)
fs <- unique(fs[file.exists(fs)])
fs <- fs[!grepl("/archived/|/00_maintenance/", fs)]

# --- Denetim -----------------------------------------------------------------
bulgular <- character()
kontrol_edilen <- 0L

for (f in fs) {
  ln <- tryCatch(readLines(f, warn = FALSE, encoding = "UTF-8"),
                 error = function(e) character())
  
  for (k in seq_along(ln)) {
    if (grepl("^\\s*#", ln[k])) next          # tam yorum satiri
    satir <- sub("#.*$", "", ln[k])           # satir sonu yorumunu at
    if (!nzchar(trimws(satir))) next
    
    m <- gregexpr(CIFT_RE, satir, perl = TRUE)[[1]]
    if (m[1] < 0) next
    
    for (esl in regmatches(satir, list(m))[[1]]) {
      id <- sub(paste0("^(TUR\\.[0-9]+\\.[0-9]+_1).*$"), "\\1", esl)
      ad <- regmatches(esl, regexpr(ILCE_RE, esl))
      if (!length(ad) || !(id %in% names(DOGRU))) next
      
      kontrol_edilen <- kontrol_edilen + 1L
      if (!grepl(DOGRU[[id]], ad, ignore.case = TRUE))
        bulgular <- c(bulgular, sprintf(
          "%s :%d | %s -> \"%s\"  (olmasi gereken: %s)",
          sub(paste0("^", here(), "/?"), "", f), k, id, ad,
          gsub("\\[g\u011f\\]", "g", DOGRU[[id]])))
    }
  }
}

cat("=== 1) ILCE ETIKET DENETIMI ===\n")
cat("Taranan dosya   :", length(fs), "\n")
cat("Kontrol edilen  :", kontrol_edilen, "esleme\n\n")

if (kontrol_edilen == 0L) {
  cat("!! Hicbir esleme yakalanamadi — CIFT_RE deseni gozden gecirilmeli.\n")
} else if (length(bulgular) == 0) {
  cat("TEMIZ — tutarsiz esleme bulunamadi.\n")
} else {
  cat("!! ", length(bulgular), " TUTARSIZLIK:\n", sep = "")
  cat(paste0("  ", bulgular, collapse = "\n"), "\n")
}

# --- 2) Uretilmis tablolarda tur atamasi -------------------------------------
BEKLENEN_TUR <- c("Kartal"    = "albopictus", "Fethiye" = "albopictus",
                  "Eğirdir"   = "albopictus", "Hopa"    = "aegypti",
                  "Zonguldak" = "aegypti")

f_eip <- here("outputs", "_canonical", "ssp245", "tables",
              "tbl_eip_sdlog_district_month.csv")

if (file.exists(f_eip)) {
  d  <- readr::read_csv(f_eip, show_col_types = FALSE)
  ic <- intersect(c("İlçe", "Ilce", "ilce", "district"), names(d))[1]
  tc <- intersect(c("Tür", "Tur", "tur", "species"), names(d))[1]
  
  if (!is.na(ic) && !is.na(tc)) {
    cat("\n=== 2) TUR ATAMASI (tbl_eip_sdlog) ===\n")
    x <- unique(data.frame(ilce = as.character(d[[ic]]),
                           tur  = as.character(d[[tc]]),
                           stringsAsFactors = FALSE))
    x$beklenen <- unname(BEKLENEN_TUR[x$ilce])
    x$durum <- ifelse(is.na(x$beklenen), "? (ad eslesmedi)",
                      ifelse(x$tur == x$beklenen, "ok", "!! HATA"))
    print(x, row.names = FALSE)
    
    cat("\n=== 3) ILCE BASINA AY SAYISI ===\n")
    tb <- as.data.frame(table(d[[ic]]), stringsAsFactors = FALSE)
    names(tb) <- c("ilce", "ay_sayisi")
    print(tb, row.names = FALSE)
    if (length(unique(tb$ay_sayisi)) > 1)
      cat("Not: Esit olmayan sayilar termal esik altinda kalan aylardir;\n",
          "     tablo altina aciklayici not dusulmelidir.\n", sep = "")
    else
      cat("Tum ilcelerde esit ay sayisi.\n")
  } else {
    cat("\n=== 2) TUR ATAMASI ===\nSutun adlari taninmadi:",
        paste(names(d), collapse = ", "), "\n")
  }
} else {
  cat("\n=== 2) TUR ATAMASI ===\nDosya yok:", f_eip, "\n")
}

# --- 3) Kanonik ciktida tur atamasi ------------------------------------------
BEKLENEN_ID <- c("TUR.40.25_1" = "albopictus", "TUR.59.4_1" = "albopictus",
                 "TUR.39.3_1"  = "albopictus", "TUR.10.4_1" = "aegypti",
                 "TUR.81.6_1"  = "aegypti")

cat("\n=== 4) KANONIK CIKTIDA TUR ATAMASI ===\n")
for (s in c("ssp126", "ssp245", "ssp585")) {
  f <- here("outputs", "_canonical", s, "simulation",
            "ctmc_spark_horizon_2025_2075_rep1000.csv")
  if (!file.exists(f)) { cat("  ", s, ": dosya yok\n", sep = ""); next }
  d <- readr::read_csv(f, show_col_types = FALSE)
  hata <- d$district_id[!mapply(function(i, sp)
    isTRUE(grepl(BEKLENEN_ID[[i]], sp)), d$district_id, d$species)]
  cat("  ", s, ": ", if (length(hata) == 0) "ok"
      else paste("!! HATA ->", paste(hata, collapse = ", ")), "\n", sep = "")
}