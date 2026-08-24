# =============================================================================
# 07_rebuild_oat_tables.R                                  [FAZ 2 - ADIM 1]
# -----------------------------------------------------------------------------
# Amac : OAT tablolarini dondurulmus _canonical/{ssp}/oat/ ciktilarindan
#        YENIDEN URETIR. Simulasyon KOSULMAZ.
#
# Neden: sensitivity_ctmc_mc.R'nin urettigi sensitivity_tornado.csv
#        `mean_delta_pct` metrigini kullaniyor. Baz deger ilcelere gore
#        1e-9 ile 1 arasinda degistiginden goreli % anlamsiz buyuyor
#        (or. +9.238.551%). Yerine ilce-bazli yuzde-puan (Delta pp) uretilir.
#
# Ayrica: lambda_local(i=1) m, beta_vh ve IP'de DOGRUSAL oldugundan
#        m_080 = beta_minus_20 = ip_minus_20  (etkin carpan 0,80)
#        m_120 = beta_plus_20  = ip_plus_20   (etkin carpan 1,20)
#        Bu dejenerasyon acikca etiketlenir.
#
# Cikti:
#   _canonical/{ssp}/oat/tbl_oat_pufuk.csv      (ilce x senaryo, P_ufuk)
#   _canonical/{ssp}/oat/tbl_oat_delta_pp.csv   (ilce x senaryo, Delta pp)
#   _canonical/tables/tbl_oat_all_ssp.csv       (birlesik, tez Tablo 5.1)
#   _canonical/tables/tbl_oat_tornado_pp.csv    (etkin carpan sinifi bazinda)
#   _canonical/cross_scenario/fig_oat_tornado_pp.png
#   _canonical/cross_scenario/fig_oat_m_profile_pp.png
# =============================================================================

library(here); library(dplyr); library(readr); library(tidyr)
library(purrr); library(tibble); library(ggplot2); library(digest); library(stringr)

CANON <- here("outputs", "_canonical")

SSPS <- c(ssp126 = "SSP1-2.6", ssp245 = "SSP2-4.5", ssp585 = "SSP5-8.5")
DIST <- c(TUR.40.25_1 = "Kartal (Istanbul)", TUR.59.4_1 = "Fethiye (Mugla)",
          TUR.10.4_1  = "Hopa (Artvin)",     TUR.81.6_1 = "Zonguldak",
          TUR.39.3_1  = "Egirdir (Isparta)")

# Senaryo -> etkin carpan ve parametre ailesi
SCEN <- tribble(
  ~scenario,        ~carpan, ~aile,     ~etiket,
  "base",             1.00,  "baz",     "Baz (m=1,00)",
  "m_050",            0.50,  "m",       "m = 0,50",
  "m_080",            0.80,  "m",       "m = 0,80",
  "m_120",            1.20,  "m",       "m = 1,20",
  "m_200",            2.00,  "m",       "m = 2,00",
  "beta_minus_20",    0.80,  "beta_vh", "beta_vh -%20 (0,24)",
  "beta_plus_20",     1.20,  "beta_vh", "beta_vh +%20 (0,36)",
  "ip_minus_20",      0.80,  "ip",      "IP -%20 (4 gun)",
  "ip_plus_20",       1.20,  "ip",      "IP +%20 (6 gun)"
)

# Salt-okunur kilidi ac
if (.Platform$OS.type == "windows")
  invisible(suppressWarnings(system2("attrib",
    c("-R", shQuote(file.path(CANON, "*.*")), "/S"), stdout = FALSE, stderr = FALSE)))

# --- Oku ---------------------------------------------------------------------
oat <- map_dfr(names(SSPS), function(s) {
  map_dfr(SCEN$scenario, function(sc) {
    f <- file.path(CANON, s, "oat", sc, "ctmc_spark_horizon_2025_2075_rep1000.csv")
    if (!file.exists(f)) { warning("YOK: ", f, call. = FALSE); return(tibble()) }
    d <- read_csv(f, show_col_types = FALSE)
    pc <- if ("p_ge1_major_mean" %in% names(d)) "p_ge1_major_mean" else "p_ge1_major"
    tibble(ssp_code = s, ssp = SSPS[[s]], scenario = sc,
           district_id = d$district_id, district = unname(DIST[d$district_id]),
           species = d$species, P = d[[pc]])
  })
})

stopifnot(nrow(oat) > 0)
oat <- oat %>% left_join(SCEN, by = "scenario")

# --- Baz degere gore fark ----------------------------------------------------
base_tbl <- oat %>% filter(scenario == "base") %>%
  select(ssp_code, district_id, P_base = P)

oat <- oat %>% left_join(base_tbl, by = c("ssp_code", "district_id")) %>%
  mutate(delta_pp  = 100 * (P - P_base),               # yuzde PUAN
         log10_kat = ifelse(P > 0 & P_base > 0, log10(P / P_base), NA_real_))

# --- Bicimlendirici (1e-9 ile 1 arasi degerler icin) -------------------------
fmt_p <- function(x) {
  ifelse(is.na(x), "-",
    ifelse(x >= 0.9999,  ">99,99%",
    ifelse(x >= 0.01,    sub("\\.", ",", sprintf("%.2f%%", 100 * x)),
    ifelse(x >= 1e-4,    sub("\\.", ",", sprintf("%.4f%%", 100 * x)),
    ifelse(x <= 0,       "~0",
                         sub("e", "\u00d710^", formatC(x, format = "e", digits = 2)))))))
}

# =============================================================================
# CIKTI 1-2: SSP bazinda ilce x senaryo tablolari
# =============================================================================
for (s in names(SSPS)) {
  sub <- oat %>% filter(ssp_code == s)

  wide_p <- sub %>% select(district, scenario, P) %>%
    pivot_wider(names_from = scenario, values_from = P) %>%
    select(district, all_of(SCEN$scenario))
  write_csv(wide_p, file.path(CANON, s, "oat", "tbl_oat_pufuk.csv"))

  wide_pp <- sub %>% filter(scenario != "base") %>%
    select(district, scenario, delta_pp) %>%
    pivot_wider(names_from = scenario, values_from = delta_pp)
  write_csv(wide_pp, file.path(CANON, s, "oat", "tbl_oat_delta_pp.csv"))
}

# =============================================================================
# CIKTI 3: Birlesik tablo (tez Tablo 5.1) — bicimlendirilmis
# =============================================================================
tbl_all <- oat %>%
  mutate(P_fmt = fmt_p(P)) %>%
  select(ssp, district, scenario, P_fmt) %>%
  pivot_wider(names_from = scenario, values_from = P_fmt) %>%
  select(Senaryo = ssp, Ilce = district,
         `m=1,00 (baz)` = base, `m=0,50` = m_050, `m=0,80` = m_080,
         `m=1,20` = m_120, `m=2,00` = m_200,
         `beta-20%` = beta_minus_20, `beta+20%` = beta_plus_20,
         `IP-20%` = ip_minus_20, `IP+20%` = ip_plus_20) %>%
  arrange(Senaryo, Ilce)

write_csv(tbl_all, file.path(CANON, "tables", "tbl_oat_all_ssp.csv"))

# =============================================================================
# CIKTI 4: Tornado — etkin carpan sinifi bazinda, Delta pp ile
# =============================================================================
# Dejenerasyon kontrolu: ayni carpanda ayni sonuc mu?
deg_check <- oat %>% filter(scenario != "base", carpan %in% c(0.80, 1.20)) %>%
  group_by(ssp, district, carpan) %>%
  summarise(n_uniq = n_distinct(signif(P, 10)), .groups = "drop")

cat("=== DOGRUSALLIK DEJENERASYONU KONTROLU ===\n")
cat("Ayni etkin carpanda farkli deger sayisi (1 = tam dejenere):\n")
deg_check %>% count(carpan, n_uniq) %>% as.data.frame() %>% print(row.names = FALSE)

tornado <- oat %>%
  filter(scenario != "base") %>%
  group_by(ssp, district, carpan) %>%
  summarise(P = first(P), P_base = first(P_base),
            delta_pp = first(delta_pp),
            senaryolar = paste(sort(unique(scenario)), collapse = " = "),
            .groups = "drop") %>%
  mutate(sinif = case_when(
    carpan == 0.50 ~ "0,50x (m)",
    carpan == 0.80 ~ "0,80x (m / beta_vh / IP)",
    carpan == 1.20 ~ "1,20x (m / beta_vh / IP)",
    carpan == 2.00 ~ "2,00x (m)",
    TRUE ~ as.character(carpan)
  ))

write_csv(tornado, file.path(CANON, "tables", "tbl_oat_tornado_pp.csv"))

# =============================================================================
# CIKTI 5-6: Figurler
# =============================================================================
dir.create(file.path(CANON, "cross_scenario"), recursive = TRUE, showWarnings = FALSE)

# Tornado: ilce bazli Delta pp (goreli % DEGIL)
p1 <- tornado %>%
  mutate(sinif = factor(sinif, levels = c("0,50x (m)", "0,80x (m / beta_vh / IP)",
                                          "1,20x (m / beta_vh / IP)", "2,00x (m)")),
         district = factor(district, levels = c("Kartal (Istanbul)", "Hopa (Artvin)",
                                                "Zonguldak", "Fethiye (Mugla)",
                                                "Egirdir (Isparta)"))) %>%
  ggplot(aes(x = sinif, y = delta_pp, fill = delta_pp > 0)) +
  geom_col() +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  coord_flip() +
  facet_grid(district ~ ssp, scales = "free_x") +
  scale_fill_manual(values = c(`TRUE` = "#d7191c", `FALSE` = "#2c7bb6"), guide = "none") +
  labs(x = NULL, y = "Baz senaryoya gore degisim (yuzde puan)",
       title = "OAT duyarlilik: etkin carpan sinifi bazinda",
       subtitle = "lambda m, beta_vh ve IP'de dogrusal oldugundan esit oranli pertubasyonlar ozdestir") +
  theme_bw(base_size = 9) +
  theme(strip.text.y = element_text(angle = 0, size = 7),
        plot.subtitle = element_text(size = 7.5, colour = "grey30"))

ggsave(file.path(CANON, "cross_scenario", "fig_oat_tornado_pp.png"),
       p1, width = 10, height = 7, dpi = 300)

# m profili — log olcek, ilce bazli
p2 <- oat %>% filter(aile %in% c("m", "baz")) %>%
  mutate(m = ifelse(scenario == "base", 1.00, carpan),
         P_plot = pmax(P, 1e-12)) %>%
  ggplot(aes(x = m, y = P_plot, colour = district)) +
  geom_line(linewidth = 0.6) + geom_point(size = 1.4) +
  geom_vline(xintercept = 1, linetype = "dashed", linewidth = 0.3) +
  scale_y_log10(labels = function(x) formatC(x, format = "e", digits = 0)) +
  facet_wrap(~ ssp) +
  labs(x = "Sivrisinek/insan orani (m)", y = expression(P[ufuk]~"(log olcek)"),
       colour = "Ilce",
       title = "m parametresinin 50 yillik kumulatif risk uzerindeki etkisi",
       subtitle = "Kesikli cizgi: baz senaryo (m = 1,00). Alt sinir 1e-12'de kirpilmistir.") +
  theme_bw(base_size = 9) +
  theme(legend.position = "bottom",
        plot.subtitle = element_text(size = 7.5, colour = "grey30"))

ggsave(file.path(CANON, "cross_scenario", "fig_oat_m_profile_pp.png"),
       p2, width = 10, height = 5, dpi = 300)

# =============================================================================
# MANIFEST guncelle
# =============================================================================
yeni <- c(
  as.vector(outer(names(SSPS), c("tbl_oat_pufuk.csv", "tbl_oat_delta_pp.csv"),
                  function(s, f) file.path(CANON, s, "oat", f))),
  file.path(CANON, "tables", "tbl_oat_all_ssp.csv"),
  file.path(CANON, "tables", "tbl_oat_tornado_pp.csv"),
  file.path(CANON, "cross_scenario", "fig_oat_tornado_pp.png"),
  file.path(CANON, "cross_scenario", "fig_oat_m_profile_pp.png")
)

man  <- read_csv(file.path(CANON, "MANIFEST.csv"), show_col_types = FALSE)
newm <- tibble(
  rel_path  = sub(paste0("^", CANON, "/?"), "", yeni),
  source    = "07_rebuild_oat_tables.R (turetilmis)",
  bytes     = file.info(yeni)$size,
  sha256    = map_chr(yeni, ~ digest(file = .x, algo = "sha256")),
  frozen_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
)
out <- bind_rows(man, newm) %>% distinct(rel_path, .keep_all = TRUE)
write_csv(out, file.path(CANON, "MANIFEST.csv"))

# =============================================================================
# RAPOR
# =============================================================================
cat("\n=== TABLO 5.1 (tez metnine) ===\n")
as.data.frame(tbl_all) %>% print(row.names = FALSE)

cat("\n=== TORNADO (Delta pp) — Kartal ===\n")
tornado %>% filter(district == "Kartal (Istanbul)") %>%
  select(ssp, sinif, P_base, P, delta_pp) %>%
  mutate(across(c(P_base, P), ~ signif(.x, 4)), delta_pp = round(delta_pp, 2)) %>%
  as.data.frame() %>% print(row.names = FALSE)

cat("\nManifest: ", nrow(out), " dosya (", nrow(newm), " yeni)\n", sep = "")
cat("Yazildi:\n"); cat(paste0("  ", sub(paste0("^", CANON, "/?"), "", yeni), collapse = "\n"), "\n")
