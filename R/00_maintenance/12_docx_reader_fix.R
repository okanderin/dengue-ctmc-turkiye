# =============================================================================
# R/00_maintenance/12_docx_reader_fix.R
#
# SORUN
#   03_consistency_check.R su satiri uretti:
#       docx:tez   okunamadi (officer hatasi)   [WARN]
#   Yani 15 ERROR'un tamami pntd'den geldi; TEZ HIC TARANMADI.
#   Rapor "docx:tez'de yalnizca 1 WARN var" gorunumu verdigi icin tezin
#   temiz oldugu YANILGISINA yol acar — denetim orada koru.
#
# NEDEN
#   officer::docx_summary() 5,1 MB'lik word/document.xml uzerinde basarisiz
#   oluyor (tezim.docx 51 parca + glossary; pntd 0,8 MB ve sorunsuz okunuyor).
#   read_docx_text() hatayi tryCatch ile yutup NA donduruyor, bu yuzden
#   sessizce WARN'a dusuyor.
#
# COZUM
#   officer'i atla: .docx zaten bir ZIP arsividir. document.xml + tablolar +
#   dipnotlar dogrudan xml2 ile okunur. Bagimliligi az, buyuk dosyada
#   hizli ve hata vermez. officer basarili olursa yine o kullanilir;
#   basarisiz olursa bu yedege duser ve DURUMU YUKSEK SESLE bildirir.
#
# KULLANIM
#   03_consistency_check.R icindeki read_docx_text() tanimini (yaklasik
#   satir 311-318) SILIN ve yerine bu dosyayi source edin:
#
#       source(here("R", "00_maintenance", "12_docx_reader_fix.R"))
#
#   ya da asagidaki fonksiyon govdesini dogrudan yapistirin.
# =============================================================================

if (!requireNamespace("xml2", quietly = TRUE))
  stop("Paket 'xml2' gerekli: install.packages('xml2')", call. = FALSE)

# --- Yedek okuyucu: .docx ZIP'ini dogrudan ac -------------------------------
.docx_text_zip <- function(path) {
  tmp <- file.path(tempdir(), paste0("docx_", basename(tools::file_path_sans_ext(path))))
  unlink(tmp, recursive = TRUE)
  dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  parts <- c("word/document.xml", "word/footnotes.xml", "word/endnotes.xml")
  ok <- suppressWarnings(tryCatch({
    utils::unzip(path, files = parts, exdir = tmp, junkpaths = FALSE)
    TRUE
  }, error = function(e) FALSE))
  if (!ok) return(NA_character_)

  W <- c(w = "http://schemas.openxmlformats.org/wordprocessingml/2006/main")
  chunks <- character(0)

  for (p in parts) {
    f <- file.path(tmp, p)
    if (!file.exists(f)) next
    doc <- tryCatch(xml2::read_xml(f), error = function(e) NULL)
    if (is.null(doc)) next
    # Her <w:p> bir paragraf; icindeki tum <w:t> dugumleri birlestirilir.
    # Tablo hucreleri de <w:p> icerdiginden otomatik olarak kapsanir.
    ps <- xml2::xml_find_all(doc, ".//w:p", ns = W)
    txt <- vapply(ps, function(node) {
      ts <- xml2::xml_find_all(node, ".//w:t", ns = W)
      if (!length(ts)) return("")
      paste0(xml2::xml_text(ts), collapse = "")
    }, character(1))
    txt <- txt[nzchar(trimws(txt))]
    chunks <- c(chunks, txt)
  }

  if (!length(chunks)) return(NA_character_)
  paste(chunks, collapse = "\n")
}

# --- Ana okuyucu: once officer, sonra ZIP yedegi ----------------------------
read_docx_text <- function(path) {
  if (is.na(path) || !file.exists(path)) return(NA_character_)

  out <- NA_character_
  if (requireNamespace("officer", quietly = TRUE)) {
    out <- tryCatch({
      s <- officer::docx_summary(officer::read_docx(path))
      paste(s$text[!is.na(s$text)], collapse = "\n")
    }, error = function(e) {
      message("[docx] officer basarisiz (", basename(path), "): ",
              conditionMessage(e), " -> ZIP yedegine geciliyor")
      NA_character_
    })
  }

  if (is.na(out) || !nzchar(out)) {
    out <- .docx_text_zip(path)
    if (!is.na(out))
      message("[docx] ", basename(path), " ZIP yedegiyle okundu (",
              format(nchar(out), big.mark = "."), " karakter)")
  }

  if (is.na(out))
    warning("[docx] HIC OKUNAMADI: ", path,
            "  — bu dokuman DENETLENMEDI, raporu temiz sanmayin.", call. = FALSE)
  out
}

# =============================================================================
# EK: scan_docx() icinde okunamayan dokuman WARN degil ERROR olmali.
# 03_consistency_check.R'de su satiri:
#
#     return(tibble(check = paste0("docx:", label),
#                   detail = "okunamadi (officer hatasi)", severity = "WARN"))
#
# sununla degistirin:
#
#     return(tibble(check = paste0("docx:", label),
#                   detail = "OKUNAMADI — bu dokuman DENETLENMEDI",
#                   severity = "ERROR"))
#
# Gerekce: taranamayan bir dokuman "sorunsuz" degildir. WARN olarak kalirsa
# raporun alt satirindaki "ERROR: 15" sayisi tezin durumu hakkinda hicbir
# sey soylemez ama soyluyormus gibi okunur.
# =============================================================================

# --- Hizli dogrulama --------------------------------------------------------
if (identical(environment(), globalenv())) {
  cands <- c(
    here::here("tezim.docx"),
    here::here("manuscript", "tezim.docx"),
    "C:/Users/oderin/OneDrive/00__ongoing/00000_ongoing_studies/tezim/yayın/tezim.docx",
    here::here("manuscript", "PLOS_NTD_final.docx"),
    "C:/Users/oderin/OneDrive/00__ongoing/00000_ongoing_studies/tezim/yayın/PLOS_NTD_final.docx"
  )
  cat("\n=== read_docx_text() testi ===\n")
  for (p in cands[file.exists(cands)]) {
    t <- read_docx_text(p)
    cat(sprintf("  %-55s %s karakter\n", basename(p),
                if (is.na(t)) "BASARISIZ" else format(nchar(t), big.mark = ".")))
  }
}
