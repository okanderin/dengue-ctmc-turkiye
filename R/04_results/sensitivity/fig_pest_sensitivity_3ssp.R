# =========================================================
# fig_pest_sensitivity_3ssp.R
# Sekil 6.4.3 — P_est'in SSP senaryolarina duyarliligi
# Ilceye gore facet, senaryoya gore renk; yillik ortalama P_est (log).
# Ince cizgi: ham yillik deger; kalin cizgi: loess trend.
# Onkosul: init.R / paths.R sourced (here, theme_thesis, DIR_OUTPUT_CROSS)
# =========================================================

library(readr); library(dplyr); library(ggplot2); library(here)

SSP_MAP <- c(ssp126 = "SSP1-2.6", ssp245 = "SSP2-4.5", ssp585 = "SSP5-8.5")

# Ilce etiketleri (init.R'de DISTRICT_LABELS varsa onu kullanabilirsiniz)
DLAB <- c(
  "TUR.39.3_1"  = "Egirdir (Isparta)",
  "TUR.59.4_1"  = "Fethiye (Mugla)",
  "TUR.10.4_1"  = "Hopa (Artvin)",
  "TUR.40.25_1" = "Kartal (Istanbul)",
  "TUR.81.6_1"  = "Zonguldak"
)

# Senaryo renkleri (init.R'de COL_SSP varsa onu kullanin)
COL_SSP <- c("SSP1-2.6" = "#1B9E77", "SSP2-4.5" = "#E6AB02", "SSP5-8.5" = "#E63946")

# ---------------------------------------------------------
# Veri: yillik CSV'yi her senaryo icin oku, birlestir.
# ONEMLI: Tablo 6.4.2 ile ayni tahmin edici (E[P_est], ctmc_spark.R non-rep)
# kullanilmali. Once non-rep dosyayi dener, yoksa _rep1000'e duser.
# ---------------------------------------------------------
read_yearly <- function(s) {
  cand <- c(
    here("outputs", s, "simulation", "ctmc_spark_yearly_2025_2075.csv"),         # non-rep (tercih)
    here("outputs", s, "simulation", "ctmc_spark_yearly_2025_2075_rep1000.csv")   # yedek
  )
  path <- cand[file.exists(cand)][1]
  if (is.na(path)) stop("Yillik CSV bulunamadi: ", s)
  read_csv(path, show_col_types = FALSE) %>%
    transmute(
      district_id,
      year,
      p_est_year = mean_p_est_year_mean,
      Senaryo    = unname(SSP_MAP[[s]])
    )
}

df <- bind_rows(lapply(names(SSP_MAP), read_yearly)) %>%
  mutate(
    ilce    = factor(DLAB[district_id], levels = unname(DLAB)),
    Senaryo = factor(Senaryo, levels = unname(SSP_MAP))
  ) %>%
  filter(is.finite(p_est_year), p_est_year > 0)

# ---------------------------------------------------------
# Figur
# ---------------------------------------------------------
fig_pest_sens <- ggplot(df, aes(year, p_est_year, colour = Senaryo)) +
  geom_line(alpha = 0.30, linewidth = 0.35) +                          # ham yillik
  geom_smooth(method = "loess", se = FALSE, span = 0.75, linewidth = 1.1) +  # trend
  facet_wrap(~ ilce, scales = "free_y") +
  scale_y_log10(labels = scales::label_scientific()) +
  scale_colour_manual(values = COL_SSP, name = "Senaryo") +
  labs(x = "Yil",
       y = expression(bar(P)[est] ~ "(yillik ort., log)")) +
  theme_thesis() +
  theme(legend.position = "bottom")

ggsave(
  filename = here(DIR_OUTPUT_CROSS, "fig_pest_sensitivity_3ssp.png"),
  plot = fig_pest_sens, width = 10, height = 6, dpi = 300
)
message("Kaydedildi: fig_pest_sensitivity_3ssp.png")
