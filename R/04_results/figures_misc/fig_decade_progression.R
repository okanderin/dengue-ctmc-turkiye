# ==========================================================
# fig_decade_progression.R
# Onyillik otokton salgin riski progresyonu — UC SSP senaryosu (facet)
# DUZELTMELER:
#  (1) Degerler artik KANONIK CSV'den (yillik p_ge1_major) okunuyor;
#      elle gomulu tablo kaldirildi. Metrik = YILLIK >=1 major olasilik
#      (Sekil 6.5.1.2 ve Tablo 6.5.1.1 ile tutarli).
#  (2) Turkce onyil etiketleri unlu uyumuna gore ("'lar"/"'ler");
#      eski paste0(...,"'ler") NA kategorisi olusturuyordu.
# Onkosul: init.R/paths.R sourced (here). theme yoksa theme_minimal kullanilir.
# ==========================================================

suppressPackageStartupMessages({
  library(ggplot2); library(dplyr); library(tidyr); library(readr)
  library(scales); library(here)
})

SSP_MAP <- c(ssp126 = "SSP1-2.6", ssp245 = "SSP2-4.5", ssp585 = "SSP5-8.5")
DLAB <- c("TUR.40.25_1"="Kartal","TUR.10.4_1"="Hopa","TUR.59.4_1"="Fethiye",
          "TUR.81.6_1"="Zonguldak","TUR.39.3_1"="Egirdir")

# Onyil tanimi (kismi ilk/son onyil): 2020'ler=2025-29 ... 2070'ler=2070-75
decade_of <- function(y) {
  cut(y, breaks = c(2024,2029,2039,2049,2059,2069,2076),
      labels = c("2020'ler","2030'lar","2040'lar","2050'ler","2060'lar","2070'ler"),
      right = TRUE)
}

read_yearly <- function(s) {
  cand <- c(
    here("outputs", s, "simulation", "ctmc_spark_yearly_2025_2075.csv"),        # non-rep (tercih)
    here("outputs", s, "simulation", "ctmc_spark_yearly_2025_2075_rep1000.csv")  # yedek
  )
  path <- cand[file.exists(cand)][1]
  if (is.na(path)) stop("Yillik CSV bulunamadi: ", s)
  read_csv(path, show_col_types = FALSE) %>%
    transmute(district_id, year,
              risk = p_ge1_major_year_mean,
              ssp  = unname(SSP_MAP[[s]]))
}

df_long <- bind_rows(lapply(names(SSP_MAP), read_yearly)) %>%
  mutate(onyil = decade_of(year),
         ilce  = DLAB[district_id]) %>%
  group_by(ssp, ilce, onyil) %>%
  summarise(risk = mean(risk, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    onyil = factor(onyil, levels = c("2020'ler","2030'lar","2040'lar",
                                     "2050'ler","2060'lar","2070'ler")),
    ssp   = factor(ssp,  levels = unname(SSP_MAP)),
    ilce  = factor(ilce, levels = c("Kartal","Hopa","Fethiye","Zonguldak","Egirdir"))
  ) %>%
  filter(is.finite(risk), risk > 0)

pal <- c("Kartal"="#2a78d6","Hopa"="#eda100","Fethiye"="#eb6834",
         "Zonguldak"="#1baf7a","Egirdir"="#4a3aa7")
shp <- c("Kartal"=16,"Hopa"=18,"Fethiye"=15,"Zonguldak"=17,"Egirdir"=25)

p <- ggplot(df_long, aes(onyil, risk, color = ilce, group = ilce, shape = ilce)) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 2) +
  facet_wrap(~ ssp, nrow = 1) +
  scale_y_log10(
    breaks = 10^seq(-13, -1, 1),
    labels = trans_format("log10", math_format(10^.x)),
    minor_breaks = as.vector(outer(1:9, 10^seq(-14, -1, 1)))
  ) +
  scale_color_manual(values = pal, name = "Ilce") +
  scale_shape_manual(values = shp, name = "Ilce") +
  labs(
    x = "Onyil",
    y = expression("Yillik otokton salgin riski"~(log[10]~"olcek)")),
    title = "Onyillik Risk Progresyonu — Uc SSP Senaryosu"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.major = element_line(linewidth = 0.25, color = "grey80"),
    panel.grid.minor = element_line(linewidth = 0.15, color = "grey92"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

out_dir <- here("outputs", "cross_scenario")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
ggsave(file.path(out_dir, "fig_decade_progression_3ssp.png"), p,
       width = 12, height = 5, dpi = 300)
message("Kaydedildi: fig_decade_progression_3ssp.png")
