# =============================================================================
# R/00_maintenance/11_preflight_check.R
# Kosum oncesi statik denetim — hicbir analiz calistirmaz, yalnizca okur.
#
# 04b_preflight_refs.R'in yerini alir: o betik 14 Agustos'ta arsivlenen 12
# dosyayi hala ariyor ve yalnizca "bulunamadi" gurultusu uretiyordu.
# Bu surum sabit bir liste kullanmaz; TUM .R/.Rmd dosyalarini tarar.
#
# DENETIMLER
#   [1] source() / render() hedefleri gercekten var mi?
#   [2] Ayni dosya adi birden fazla dizinde mi? (surum cakismasi)
#   [3] _old / old_ / _guncel adli ikizler
#   [4] .Rproj oturum ayarlari (RestoreWorkspace / SaveWorkspace)
#   [5] Kanonik cikti agaci ve MANIFEST butunlugu
#   [6] Superseded (Bernoulli donemi) cikti kopyalari hala yerinde mi?
#
# KULLANIM
#   source("R/00_maintenance/11_preflight_check.R")
# =============================================================================

suppressPackageStartupMessages(library(rprojroot))
ROOT <- rprojroot::find_root(rprojroot::has_file("r_project_tez.Rproj"))
setwd(ROOT)

ISSUES <- list()
.add <- function(sev, check, detail)
  ISSUES[[length(ISSUES) + 1]] <<- data.frame(severity = sev, check = check,
                                              detail = detail,
                                              stringsAsFactors = FALSE)

cat(strrep("=", 70), "\n"); cat("PREFLIGHT  |  ", ROOT, "\n")
cat(strrep("=", 70), "\n\n")

SKIP <- "(^|/)(\\.git|\\.Rproj\\.user|_to_delete|archived|renv)/"
files <- list.files(".", pattern = "\\.(R|Rmd)$", recursive = TRUE,
                    full.names = FALSE)
files <- files[!grepl(SKIP, files)]
all_files <- list.files(".", recursive = TRUE, full.names = FALSE)
all_files <- all_files[!grepl("(^|/)(\\.git|\\.Rproj\\.user)/", all_files)]
by_base   <- split(all_files, basename(all_files))

cat("Taranan betik sayisi:", length(files), "\n\n")

# -----------------------------------------------------------------------------
# [1] + [2]  Dosya referanslarinin cozumlenmesi
# -----------------------------------------------------------------------------
pat <- "[\"']([A-Za-z0-9_./À-ſ-]+\\.(?:R|Rmd|csv|rds))[\"']"

for (f in files) {
  txt <- tryCatch(readLines(f, warn = FALSE, encoding = "UTF-8"),
                  error = function(e) character(0))
  for (i in seq_along(txt)) {
    line <- txt[i]
    if (grepl("^\\s*#", line)) next
    if (!grepl("source|render|read_csv|readRDS|read\\.csv|file\\.path|here\\(", line)) next
    m <- regmatches(line, gregexpr(pat, line, perl = TRUE))[[1]]
    for (raw in m) {
      p  <- gsub("^[\"']|[\"']$", "", raw)
      p  <- sub("^\\./", "", p)
      bn <- basename(p)
      if (!grepl("\\.(R|Rmd)$", bn)) next          # veri dosyalari degisken yol kullanir
      if (is.null(by_base[[bn]])) {
        .add("ERROR", "kirik-referans", sprintf("%s:%d  ->  %s  (BOYLE BIR DOSYA YOK)", f, i, p))
      } else if (!(p %in% all_files) && !any(endsWith(all_files, p))) {
        .add("WARN", "yol-farkli",
             sprintf("%s:%d  ->  %s   (gercek konum: %s)", f, i, p,
                     paste(by_base[[bn]], collapse = " | ")))
      }
      if (length(by_base[[bn]]) > 1) {
        .add("WARN", "surum-cakismasi",
             sprintf("'%s' %d ayri konumda: %s", bn, length(by_base[[bn]]),
                     paste(by_base[[bn]], collapse = " | ")))
      }
    }
  }
}

# -----------------------------------------------------------------------------
# [3] _old / old_ / _guncel ikizleri
# -----------------------------------------------------------------------------
scripts <- all_files[grepl("\\.(R|Rmd)$", all_files) & !grepl(SKIP, all_files)]
for (s in scripts) {
  b <- basename(s)
  if (grepl("(_old|_v1|_eski)\\.(R|Rmd)$|^old_", b))
    .add("WARN", "eski-surum", sprintf("%s  — arsive tasinmali", s))
}
# guncel/base ciftleri
for (s in scripts) {
  b <- basename(s)
  if (grepl("_guncel\\.(R|Rmd)$", b)) {
    base <- sub("_guncel(\\.(R|Rmd))$", "\\1", b)
    if (!is.null(by_base[[base]]))
      .add("WARN", "ikiz-surum",
           sprintf("'%s' ve '%s' birlikte duruyor — hangisinin kanonik oldugu belirsiz",
                   b, base))
  }
}

# -----------------------------------------------------------------------------
# [4] .Rproj oturum ayarlari
# -----------------------------------------------------------------------------
rp <- readLines("r_project_tez.Rproj", warn = FALSE)
if (any(grepl("^RestoreWorkspace:\\s*Yes", rp)))
  .add("ERROR", "rproj",
       "RestoreWorkspace: Yes  -> kok .RData her acilista yukleniyor; 'No' yapin")
if (any(grepl("^SaveWorkspace:\\s*Yes", rp)))
  .add("ERROR", "rproj",
       "SaveWorkspace: Yes  -> her cikista .RData yaziliyor; 'No' yapin")
if (file.exists(".RData"))
  .add("WARN", "rdata",
       sprintf("Kok dizinde .RData var (%.1f MB) — temiz-ortam disiplinini bozar",
               file.info(".RData")$size / 1e6))

# -----------------------------------------------------------------------------
# [5] Kanonik agac
# -----------------------------------------------------------------------------
CANON <- file.path("outputs", "_canonical")
if (!dir.exists(CANON)) {
  .add("ERROR", "kanonik", "outputs/_canonical/ yok — 02_freeze_canonical.R kosun")
} else {
  man <- file.path(CANON, "MANIFEST.csv")
  if (!file.exists(man)) {
    .add("ERROR", "kanonik", "MANIFEST.csv yok")
  } else {
    mm <- utils::read.csv(man, stringsAsFactors = FALSE)
    if ("rel_path" %in% names(mm)) {
      missing <- mm$rel_path[!file.exists(file.path(CANON, mm$rel_path))]
      if (length(missing))
        .add("ERROR", "kanonik",
             sprintf("MANIFEST'te olup diskte olmayan %d dosya (ilk 5: %s)",
                     length(missing), paste(utils::head(missing, 5), collapse = ", ")))
      else
        cat("  [OK] MANIFEST:", nrow(mm), "dosyanin tamami yerinde\n")
    }
  }
  prov <- file.path(CANON, "PROVENANCE.txt")
  if (file.exists(prov))
    cat("  [OK] PROVENANCE:", sub(".*: ", "", grep("frozen_at",
        readLines(prov, warn = FALSE), value = TRUE)[1]), "\n")
}

# -----------------------------------------------------------------------------
# [6] Superseded (Bernoulli donemi) kopyalar
# -----------------------------------------------------------------------------
susp <- c(
  "outputs/Yeni_klasor",
  file.path("outputs", c("ssp126","ssp245","ssp585"), "simulation", "Yeni_klasor"),
  file.path("outputs", c("ssp126","ssp245","ssp585"), "tables",     "Yeni_klasor")
)
for (d in susp) if (dir.exists(d))
  .add("WARN", "superseded-cikti",
       sprintf("%s  — Bernoulli donemi kopyasi olabilir; 10_cleanup_workspace.R", d))

# Shiny veri tazeligi: kanonik ile shiny horizon dosyalarini karsilastir
for (s in c("ssp126", "ssp245", "ssp585")) {
  a <- file.path(CANON, s, "simulation", "ctmc_spark_horizon_2025_2075_rep1000.csv")
  b <- file.path("shiny_dengue_app", "outputs", s, "simulation",
                 "ctmc_spark_horizon_2025_2075_rep1000.csv")
  if (file.exists(a) && file.exists(b)) {
    ha <- tools::md5sum(a); hb <- tools::md5sum(b)
    if (unname(ha) != unname(hb))
      .add("ERROR", "shiny-bayat",
           sprintf("shiny_dengue_app/outputs/%s  kanonikten FARKLI — eski sayilar yayinlaniyor olabilir", s))
  } else if (file.exists(b) && !file.exists(a)) {
    .add("WARN", "shiny-bayat",
         sprintf("Kanonik karsiligi bulunamadi: %s", a))
  }
}

# -----------------------------------------------------------------------------
# RAPOR
# -----------------------------------------------------------------------------
df <- if (length(ISSUES)) unique(do.call(rbind, ISSUES)) else
      data.frame(severity = character(), check = character(), detail = character())

cat("\n", strrep("-", 70), "\n", sep = "")
if (!nrow(df)) {
  cat("TEMIZ — hicbir sorun bulunmadi.\n")
} else {
  for (sev in c("ERROR", "WARN")) {
    sub <- df[df$severity == sev, ]
    if (!nrow(sub)) next
    cat("\n### ", sev, " (", nrow(sub), ")\n", sep = "")
    for (ck in unique(sub$check)) {
      cat("\n  [", ck, "]\n", sep = "")
      for (d in sub$detail[sub$check == ck]) cat("    - ", d, "\n", sep = "")
    }
  }
}
cat("\n", strrep("-", 70), "\n", sep = "")

out <- file.path("outputs", "_audit", "preflight_report.csv")
dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
utils::write.csv(df, out, row.names = FALSE, fileEncoding = "UTF-8")
cat("Rapor:", out, "\n")

invisible(df)
