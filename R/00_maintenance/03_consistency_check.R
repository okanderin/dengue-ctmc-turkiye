# =============================================================================
# 03_consistency_check.R                                  [SURUM 3 - DENETLE]
# -----------------------------------------------------------------------------
# (a) MANIFEST hash butunlugu
# (b) Kanonik P_ufuk tablosu (metne yazilacak degerler)
# (c) Analitik vs MC uyumu
# (d) OAT ozeti + dogrusallik dejenerasyonu
# (e) Dokuman taramasi: superseded sayilar, yanlis kaynaklar, coken argumanlar
# (f) POZITIF kontrol: yeni degerler dokumanlarda var mi?
#
# SURUM 3 DUZELTMELERI
#   [1] BANNED listesi tezin GERCEK icerigine gore yenilendi. Onceki surum
#       ana hattin Bernoulli degerlerini (28,6 / 36,05 / 48,38) ariyordu;
#       oysa tez OAT Bernoulli degerlerini (29,4 / 36,8 / 49,4) tasiyor.
#   [2] Ust simge gosterimi (10⁻¹, 10⁻⁴ ...) desteklendi; docx metninde
#       bilimsel gosterim Unicode ust simge ile yaziliyor.
#   [3] Sadece "var/yok" yerine ESLESME SAYISI ve baglam raporlaniyor.
#   [4] Coken anlati kaliplari eklendi ("kesinlige yaklasmamaktadir",
#       "tumleyen Bernoulli carpimi", "bit duzeyinde ozdes", "Elli yillik",
#       "1,7 katlik", "bes buyukluk mertebesi").
#   [5] Zaten duzeltilmis kalemler (ICC 0,977 / %37,8 / T_opt 19) INFO
#       seviyesine alindi; bulunursa yine raporlanir ama ERROR sayilmaz.
#   [6] find_doc() fallback olarak korundu; DOCS yolu bulunamazsa arar.
#   [7] Dokuman bazinda beklenen kanonik degerlerin VARLIGI kontrol ediliyor.
#
# Cikti: outputs/_audit/consistency_report.csv   (ERROR satiri yoksa gecti)
#        outputs/_audit/canonical_values.csv
#        outputs/_audit/canonical_oat.csv
#        outputs/_audit/docx_scan_detail.csv
# =============================================================================

library(here); library(dplyr); library(readr); library(digest)
library(purrr); library(stringr); library(tidyr); library(tibble)

CANON     <- here("outputs", "_canonical")
AUDIT_DIR <- here("outputs", "_audit")
dir.create(AUDIT_DIR, recursive = TRUE, showWarnings = FALSE)

SSPS <- c(ssp126 = "SSP1-2.6", ssp245 = "SSP2-4.5", ssp585 = "SSP5-8.5")
DIST <- c(TUR.40.25_1 = "Kartal",    TUR.59.4_1 = "Fethiye", TUR.10.4_1 = "Hopa",
          TUR.81.6_1  = "Zonguldak", TUR.39.3_1 = "Egirdir")

issues <- tibble(check = character(), detail = character(), severity = character())
add <- function(df, check, detail, severity = "ERROR")
  bind_rows(df, tibble(check = check, detail = detail, severity = severity))

pick_p_col <- function(d) {
  cand <- c("p_ge1_major_mean", "p_ge1_major",
            grep("^p_ge1_major", names(d), value = TRUE))
  hit <- cand[cand %in% names(d)]
  if (length(hit)) hit[1] else NA_character_
}

# =============================================================================
# (a) MANIFEST butunlugu (Optimize Edilmiş Versiyon)
# =============================================================================
man_path <- file.path(CANON, "MANIFEST.csv")

if (!file.exists(man_path)) {
  issues <- add(issues, "manifest", "MANIFEST.csv yok — 02_freeze_canonical.R kos")
} else {
  man <- read_csv(man_path, show_col_types = FALSE)
  
  # 1. file.exists()'i döngü dışında vektörize olarak çağırıyoruz
  full_paths <- file.path(CANON, man$rel_path)
  exists_vec <- file.exists(full_paths)
  
  # 2. Hata toleranslı (safe) hash hesaplama fonksiyonu
  safe_digest <- function(x) {
    tryCatch(digest(file = x, algo = "sha256"), 
             error = function(e) NA_character_)
  }
  
  drift <- man %>%
    mutate(
      full = full_paths,
      file_exists = exists_vec,
      # Sadece var olan dosyalar için hash hesapla, yoksa doğrudan NA ata
      now = if_else(file_exists, map_chr(full, safe_digest), NA_character_)
    ) %>%
    filter(!file_exists | is.na(now) | now != sha256)
  
  cat("=== MANIFEST ===\n")
  cat(sprintf("Dosya: %d | kayma/eksik: %d\n", nrow(man), nrow(drift)))
  
  if (nrow(drift) > 0) {
    drift_files <- head(drift$rel_path, 10)
    msg <- sprintf("hash kaymasi/eksik: %s%s", 
                   paste(drift_files, collapse = "; "), 
                   if (nrow(drift) > 10) sprintf(" (+%d daha)", nrow(drift) - 10) else "")
    
    issues <- add(issues, "manifest", msg)
    cat("!! Kayan dosyalar:\n")
    cat(paste0("   ", drift_files, collapse = "\n"), "\n")
  }
}

# =============================================================================
# (b) Kanonik P_ufuk tablosu
# =============================================================================
canon_tbl <- map_dfr(names(SSPS), function(s) {
  f <- file.path(CANON, s, "simulation", "ctmc_spark_horizon_2025_2075_rep1000.csv")
  if (!file.exists(f)) return(tibble())
  d <- read_csv(f, show_col_types = FALSE); pc <- pick_p_col(d)
  if (is.na(pc)) return(tibble())
  tibble(ssp = SSPS[[s]], district = unname(DIST[d$district_id]),
         species = d$species, P_ufuk = d[[pc]], Lambda_import = d$Lambda_import)
})

if (nrow(canon_tbl) == 0) {
  issues <- add(issues, "canonical", "MC horizon dosyalari okunamadi")
} else {
  cat("\n=== KANONIK P_ufuk (metne yazilacak) ===\n")
  canon_tbl %>%
    mutate(P_ufuk_pct = signif(100 * P_ufuk, 4), P_ufuk = signif(P_ufuk, 5),
           Lambda_import = round(Lambda_import, 1)) %>%
    select(district, ssp, P_ufuk, P_ufuk_pct, Lambda_import) %>%
    arrange(district, ssp) %>% as.data.frame() %>% print(row.names = FALSE)

  write_csv(canon_tbl, file.path(AUDIT_DIR, "canonical_values.csv"))

  exp_sp <- c(Kartal = "albopictus", Fethiye = "albopictus", Egirdir = "albopictus",
              Hopa = "aegypti", Zonguldak = "aegypti")
  sp_bad <- canon_tbl %>% mutate(beklenen = unname(exp_sp[district])) %>%
    filter(!is.na(beklenen), !str_detect(species, fixed(beklenen)))
  if (nrow(sp_bad) > 0)
    issues <- add(issues, "species",
                  paste("yanlis tur:", paste(unique(sp_bad$district), collapse = ", ")))

  cat("\n--- Termal overshoot (SSP5-8.5 < SSP2-4.5) ---\n")
  canon_tbl %>% select(district, ssp, P_ufuk) %>%
    pivot_wider(names_from = ssp, values_from = P_ufuk) %>%
    mutate(overshoot = `SSP5-8.5` < `SSP2-4.5`,
           across(where(is.numeric), ~ signif(.x, 3))) %>%
    as.data.frame() %>% print(row.names = FALSE)
}

# =============================================================================
# (c) Analitik vs MC
# =============================================================================
ana_tbl <- map_dfr(names(SSPS), function(s) {
  f <- file.path(CANON, s, "analytic", "ctmc_spark_horizon_2025_2075.csv")
  if (!file.exists(f)) return(tibble())
  d <- read_csv(f, show_col_types = FALSE); pc <- pick_p_col(d)
  if (is.na(pc)) return(tibble())
  tibble(ssp = SSPS[[s]], district = unname(DIST[d$district_id]), P_analytic = d[[pc]])
})

if (nrow(ana_tbl) > 0 && nrow(canon_tbl) > 0) {
  cmp <- canon_tbl %>% select(ssp, district, P_MC = P_ufuk) %>%
    left_join(ana_tbl, by = c("ssp", "district")) %>%
    mutate(abs_diff = abs(P_MC - P_analytic),
           log10_oran = log10(pmax(P_analytic, 1e-300) / pmax(P_MC, 1e-300)))

  cat("\n=== ANALITIK vs MC (P_ufuk) ===\n")
  cmp %>% mutate(across(where(is.numeric), ~ signif(.x, 4))) %>%
    arrange(district, ssp) %>% as.data.frame() %>% print(row.names = FALSE)
  cat("Maks |fark|: ", signif(max(cmp$abs_diff, na.rm = TRUE), 3),
      " | medyan log10 oran: ", signif(median(cmp$log10_oran, na.rm = TRUE), 3),
      "\n", sep = "")

  n_bad <- sum(cmp$abs_diff > 0.02, na.rm = TRUE)
  if (n_bad > 0)
    issues <- add(issues, "analytic_vs_mc", paste("|fark| > 0.02:", n_bad, "satir"))
} else {
  issues <- add(issues, "analytic", "Analitik dosyalar okunamadi", "WARN")
}

# =============================================================================
# (d) OAT
# =============================================================================
OAT_SCEN <- c("base", "m_050", "m_080", "m_120", "m_200",
              "beta_minus_20", "beta_plus_20", "ip_minus_20", "ip_plus_20")

oat_tbl <- map_dfr(names(SSPS), function(s) {
  map_dfr(OAT_SCEN, function(sc) {
    f <- file.path(CANON, s, "oat", sc, "ctmc_spark_horizon_2025_2075_rep1000.csv")
    if (!file.exists(f)) return(tibble())
    d <- read_csv(f, show_col_types = FALSE); pc <- pick_p_col(d)
    if (is.na(pc)) return(tibble())
    tibble(ssp = SSPS[[s]], scenario = sc,
           district = unname(DIST[d$district_id]), P_ufuk = d[[pc]])
  })
})

if (nrow(oat_tbl) > 0) {
  write_csv(oat_tbl, file.path(AUDIT_DIR, "canonical_oat.csv"))

  cat("\n=== OAT — Kartal ===\n")
  oat_tbl %>% filter(district == "Kartal") %>%
    mutate(P = signif(P_ufuk, 4)) %>% select(-P_ufuk, -district) %>%
    pivot_wider(names_from = scenario, values_from = P) %>%
    as.data.frame() %>% print(row.names = FALSE)

  chk <- oat_tbl %>% filter(scenario == "base") %>%
    select(ssp, district, P_oat = P_ufuk) %>%
    left_join(canon_tbl %>% select(ssp, district, P_main = P_ufuk),
              by = c("ssp", "district")) %>%
    mutate(d = abs(P_oat - P_main))
  cat("\nOAT base vs ana kosum — maks |fark|: ",
      format(max(chk$d, na.rm = TRUE), scientific = TRUE, digits = 3), "\n", sep = "")
  if (max(chk$d, na.rm = TRUE) > 1e-12)
    issues <- add(issues, "oat_base", "OAT base ana kosumla ozdes degil")

  deg <- oat_tbl %>%
    filter(district == "Kartal",
           scenario %in% c("m_080", "beta_minus_20", "ip_minus_20")) %>%
    group_by(ssp) %>% summarise(n_uniq = n_distinct(signif(P_ufuk, 10)), .groups = "drop")
  cat("Dogrusallik dejenerasyonu (1 beklenir): ",
      paste0(deg$ssp, "=", deg$n_uniq, collapse = ", "), "\n", sep = "")
} else {
  issues <- add(issues, "oat", "OAT dosyalari okunamadi", "WARN")
}

# =============================================================================
# (e) DOKUMAN TARAMASI
# =============================================================================
find_doc <- function(fname) {
  cand <- c(here(fname), here("..", fname),
            list.files(here(), pattern = paste0("^", fname, "$"),
                       recursive = TRUE, full.names = TRUE))
  hit <- cand[file.exists(cand)]
  if (length(hit)) normalizePath(hit[1]) else NA_character_
}

resolve_doc <- function(path, fallback_name) {
  if (!is.na(path) && file.exists(path)) return(normalizePath(path))
  find_doc(fallback_name)
}

DOCS <- c(
  tez  = resolve_doc(
    "C:/Users/oderin/OneDrive/00__ongoing/00000_ongoing_studies/tezim/yayın/tezim.docx",
    "tezim.docx"),
  pntd = resolve_doc(
    "C:/Users/oderin/OneDrive/00__ongoing/00000_ongoing_studies/tezim/yayın/PLOS_NTD_final.docx",
    "PLOS_NTD_final.docx")
)

# --- Yasakli kaliplar --------------------------------------------------------
# NOT: Ust simge varyantlari icin  ×10⁻¹  ve  x10-1  formlari birlikte aranir.
SUP <- function(x) {                      # "10^-4" -> her iki gosterim
  us <- c("0"="\u2070","1"="\u00b9","2"="\u00b2","3"="\u00b3","4"="\u2074",
          "5"="\u2075","6"="\u2076","7"="\u2077","8"="\u2078","9"="\u2079")
  paste0("10[\u207b^-]?(", paste0(us[strsplit(x,"")[[1]]], collapse=""),
         "|", x, ")")
}

BANNED <- tribble(
  ~pattern, ~why, ~severity,

  # --- Bernoulli donemi P_ufuk degerleri (TEZDE FIILEN BULUNANLAR) -----------
  "%\\s*29[.,]4\\b|\\b29[.,]4\\s*%",   "Bernoulli Kartal SSP1-2.6 (yeni: %97,46)", "ERROR",
  "%\\s*36[.,]8\\b|\\b36[.,]8\\s*%",   "Bernoulli Kartal SSP2-4.5 (yeni: %99,22)", "ERROR",
  "%\\s*49[.,]4\\b|\\b49[.,]4\\s*%",   "Bernoulli Kartal SSP5-8.5 (yeni: %99,93)", "ERROR",
  "2[.,]936\\s*[×x]",                  "Bernoulli Kartal SSP1-2.6 (bilimsel gost.)", "ERROR",
  "3[.,]68[01]\\s*[×x]",               "Bernoulli Kartal SSP2-4.5 (bilimsel gost.)", "ERROR",
  "4[.,]940?\\s*[×x]\\s*10",           "Bernoulli Kartal SSP5-8.5 (bilimsel gost.)", "ERROR",
  "\\b0[.,]4940?\\b",                  "Bernoulli Kartal SSP5-8.5 (ondalik)", "ERROR",
  "1[.,]458\\s*[×x]",                  "Bernoulli Hopa SSP1-2.6 (yeni: 1,506e-3)", "ERROR",
  "1[.,]049\\s*[×x]",                  "Bernoulli Hopa SSP2-4.5 (yeni: 1,078e-2)", "ERROR",
  "2[.,]318\\s*[×x]",                  "Bernoulli Hopa SSP5-8.5 (yeni: 2,369e-2)", "ERROR",
  "%\\s*0[.,]23\\b",                   "Bernoulli Hopa SSP5-8.5 (yeni: %2,37)", "ERROR",
  "3[.,]68[56]\\s*[×x]|3[.,]69\\s*[×x]","Bernoulli Fethiye SSP5-8.5 (yeni: 3,703e-4)", "ERROR",
  "1[.,]742\\s*[×x]",                  "Bernoulli Egirdir SSP5-8.5 (yeni: 1,749e-8)", "ERROR",

  # --- Coken / gecersiz anlati ----------------------------------------------
  "q_?ithal[^)\n]{0,15}[×x]\\s*\\n?\\s*P_?est",
    "YONTEM: p_ay = q_ithal x P_est (Bernoulli) — Poisson inceltmesi olmali", "ERROR",
  "Bernoulli\\s+denemelerinin\\s+t[uü]mleyen\\s+[cç]arp[iı]m",
    "YONTEM: Bernoulli tumleyen carpimi tarifi — degistirilmeli", "ERROR",
  "Poisson[- ]binomial",
    "YONTEM: Poisson-binomial cerceve tarifi — Poisson inceltmesiyle degistirilmeli", "ERROR",
  "bit\\s+d[uü]zeyinde\\s+[oö]zde[sş]",
    "YONTEM: 'tekrarlar arasi varyans sifir' iddiasi artik gecersiz", "ERROR",
  "kesinli[gğ]e\\s+yakla[sş]mamaktad[iı]r",
    "TARTISMA: 'risk kesinlige yaklasmiyor' argumani cokmus (Kartal %99,9)", "ERROR",
  "[Ee]lli\\s+y[iı]ll[iı]k",
    "2025-2075 = 51 takvim yili (612 ay)", "ERROR",
  "1[.,]7\\s*kat",
    "SSP1-2.6 -> SSP5-8.5 farki artik 1,03 kat (2,5 yuzde puan)", "ERROR",
  "be[sş]\\s+b[uü]y[uü]kl[uü]k\\s+mertebe",
    "Hiyerarsi artik SEKIZ buyukluk mertebesi", "ERROR",

  # --- Kaynak / terminoloji --------------------------------------------------
  "Cheng\\s+(et\\s+al|ve\\s+ark)",     "yanlis k kaynagi — Damtew 2023", "ERROR",
  "Ryan[^)]{0,25}20\\s?17",            "Ryan yili 2019", "ERROR",
  "Tarnas[^)]{0,25}20\\s?23",          "Tarnas yili 2021", "ERROR",
  "Brady[^.]{0,60}(VPD|k_?vpd)",       "VPD kaynagi Schmidt 2018", "ERROR",
  "Bri[e\u00e8]re[^.]{0,40}kuadratik", "Briere kuadratik degil", "ERROR",
  "parametrik belirsizli[^.]{0,60}(yay|propag)",
    "n_rep parametrik belirsizlik YAYMAZ", "ERROR",

  # --- Zaten duzeltilmis olmasi beklenenler (bulunursa INFO) ----------------
  "ICC[^.,\\d]{0,15}0[.,]977",         "eski ICC (yeni: 0,843)", "INFO",
  "%\\s*37[.,]8\\b",                   "eski OLS R2 (yeni: 0,807)", "INFO",
  "T[_ ]?opt[^.,\\d]{0,20}19[.,]0",    "eski OLS T_opt ~19 C", "INFO"
)

# --- Beklenen (POZITIF) kaliplar --------------------------------------------
EXPECTED <- tribble(
  ~pattern, ~what,
  "97[.,]4[56]",                    "Kartal SSP1-2.6 = %97,46",
  "99[.,]2[12]",                    "Kartal SSP2-4.5 = %99,22",
  "99[.,]9[23]",                    "Kartal SSP5-8.5 = %99,93",
  "Poisson\\s+incelt|Poisson\\s+thinn", "Poisson inceltmesi tarifi",
  "51\\s*(takvim\\s*)?y[iı]l",      "51 takvim yili ifadesi",
  "612\\s*ay",                      "612 ay ifadesi"
)


detail_rows <- list()

scan_docx <- function(path, label) {
  if (is.na(path) || !file.exists(path))
    return(tibble(check = paste0("docx:", label),
                  detail = "dosya bulunamadi", severity = "WARN"))
  txt <- read_docx_text(path)
  if (is.na(txt))
    return(tibble(check = paste0("docx:", label),
                  detail = "okunamadi (officer hatasi)", severity = "WARN"))

  cat("\n--- ", label, " (", format(nchar(txt), big.mark = "."), " karakter) ---\n",
      sep = "")

  # Yasakli kaliplar
  hits <- map_dfr(seq_len(nrow(BANNED)), function(i) {
    n <- str_count(txt, regex(BANNED$pattern[i], ignore_case = TRUE))
    if (n == 0) return(tibble())
    ctx <- str_extract(txt, regex(paste0(".{0,45}(", BANNED$pattern[i], ").{0,45}"),
                                  ignore_case = TRUE))
    detail_rows[[length(detail_rows) + 1]] <<- tibble(
      doc = label, tur = "YASAKLI", n = n,
      aciklama = BANNED$why[i], baglam = str_squish(ctx))
    tibble(check = paste0("docx:", label),
           detail = sprintf("[%dx] %s  |  ...%s...", n, BANNED$why[i],
                            str_squish(ctx)),
           severity = BANNED$severity[i])
  })

  # Beklenen kaliplar
  eks <- map_dfr(seq_len(nrow(EXPECTED)), function(i) {
    n <- str_count(txt, regex(EXPECTED$pattern[i], ignore_case = TRUE))
    detail_rows[[length(detail_rows) + 1]] <<- tibble(
      doc = label, tur = "BEKLENEN", n = n,
      aciklama = EXPECTED$what[i], baglam = NA_character_)
    if (n > 0) return(tibble())
    tibble(check = paste0("docx:", label),
           detail = paste("EKSIK:", EXPECTED$what[i]),
           severity = "ERROR")
  })

  bind_rows(hits, eks)
}

cat("\n=== DOKUMAN TARAMASI ===\n")
for (i in seq_along(DOCS))
  cat("  ", names(DOCS)[i], ": ",
      ifelse(is.na(DOCS[i]), "BULUNAMADI", DOCS[i]), "\n", sep = "")

issues <- bind_rows(issues, map2_dfr(DOCS, names(DOCS), scan_docx))

if (length(detail_rows) > 0) {
  dt <- bind_rows(detail_rows)
  write_csv(dt, file.path(AUDIT_DIR, "docx_scan_detail.csv"))
  cat("\n--- Tarama ozeti (dokuman x tur) ---\n")
  dt %>% group_by(doc, tur) %>%
    summarise(kalem = n(), bulunan = sum(n > 0), toplam_eslesme = sum(n),
              .groups = "drop") %>%
    as.data.frame() %>% print(row.names = FALSE)
}

# =============================================================================
# RAPOR
# =============================================================================
if (.Platform$OS.type == "windows")
  invisible(suppressWarnings(system2("attrib",
    c("-R", shQuote(file.path(AUDIT_DIR, "*.*"))), stdout = FALSE, stderr = FALSE)))

write_csv(issues, file.path(AUDIT_DIR, "consistency_report.csv"))

cat("\n=== TUTARLILIK RAPORU ===\n")
n_err  <- sum(issues$severity == "ERROR")
n_warn <- sum(issues$severity == "WARN")
n_info <- sum(issues$severity == "INFO")

if (nrow(issues) == 0) {
  cat("GECTI — sorun yok.\n")
} else {
  issues %>% arrange(match(severity, c("ERROR", "WARN", "INFO"))) %>%
    mutate(detail = str_trunc(detail, 105)) %>%
    as.data.frame() %>% print(row.names = FALSE)
  cat("\nERROR: ", n_err, " | WARN: ", n_warn, " | INFO: ", n_info, "\n", sep = "")
  cat("Rapor : ", file.path(AUDIT_DIR, "consistency_report.csv"), "\n", sep = "")
  cat("Detay : ", file.path(AUDIT_DIR, "docx_scan_detail.csv"), "\n", sep = "")
  if (n_err == 0) cat("\nERROR yok — kalanlar bilgi/uyari duzeyinde.\n")
}
