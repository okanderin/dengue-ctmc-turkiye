# =============================================================================
# 06_diag_lambda_outlier.R
# (A) albopictus'ta 10^43 mertebesindeki lambda sapmasi gercek bug mu?
# (B) Sinir uyusmazligi: bir tarafta pozitif, digerinde sifir olan hucreler
# =============================================================================

library(here); library(dplyr); library(readr); library(purrr); library(tibble)

d <- map_dfr(c("ssp126","ssp245","ssp585"), function(s) {
  a <- read_csv(here("outputs","_canonical",s,"analytic",
                     "ctmc_spark_monthly_2025_2075.csv"), show_col_types = FALSE)
  m <- read_csv(here("outputs","_canonical",s,"simulation",
                     "ctmc_spark_monthly_2025_2075_rep1000.csv"), show_col_types = FALSE)
  pa <- grep("^p_establishment", names(a), value = TRUE)[1]
  la <- grep("^lambda_local_i1", names(a), value = TRUE)[1]
  tibble(ssp = s, district = m$district_id, species = m$species,
         year = m$year, month = m$month, temp = m$temp_c, rh = m$rh,
         lam_a = a[[la]], lam_m = m$lambda_local_i1_mean,
         p_a = a[[pa]],  p_m = m$p_establishment_mean)
})

# --- (A) lambda aykiri degerleri --------------------------------------------
cat("=== (A) EN BUYUK 15 LAMBDA SAPMASI ===\n")
d %>% filter(lam_a > 0, lam_m > 0) %>%
  mutate(lr = log10(lam_a / lam_m)) %>%
  arrange(desc(abs(lr))) %>% head(15) %>%
  transmute(ssp, district, year, month, species,
            temp = round(temp, 1), rh = round(rh, 1),
            lam_a = signif(lam_a, 3), lam_m = signif(lam_m, 3),
            log10_oran = round(lr, 2)) %>%
  as.data.frame() %>% print(row.names = FALSE)

cat("\n--- Sapma buyuklugune gore hucre sayisi ---\n")
d %>% filter(lam_a > 0, lam_m > 0) %>%
  mutate(lr = abs(log10(lam_a / lam_m)),
         grup = cut(lr, c(-Inf, 0.01, 0.1, 1, 10, Inf),
                    labels = c("<0.01 (ihmal)", "0.01-0.1", "0.1-1", "1-10", ">10 (BUG?)"))) %>%
  count(species, grup) %>% as.data.frame() %>% print(row.names = FALSE)

cat("\n--- Aykiri hucrelerde lambda mutlak mertebesi ---\n")
# Denormal/underflow bolgesindeyse (~1e-300) zararsiz; anlamli mertebedeyse gercek hata
d %>% filter(lam_a > 0, lam_m > 0, abs(log10(lam_a/lam_m)) > 1) %>%
  summarise(n = n(),
            min_lam_a = min(lam_a), max_lam_a = max(lam_a),
            min_lam_m = min(lam_m), max_lam_m = max(lam_m)) %>%
  as.data.frame() %>% print(row.names = FALSE)

# --- (B) Sinir uyusmazligi ---------------------------------------------------
cat("\n=== (B) SINIR UYUSMAZLIGI ===\n")
d %>% filter(xor(p_a > 0, p_m > 0)) %>%
  mutate(hangi = ifelse(p_a > 0, "yalniz_analitik", "yalniz_MC")) %>%
  count(species, hangi) %>% as.data.frame() %>% print(row.names = FALSE)

cat("\n--- Yalniz analitik pozitif olan hucrelerde P_analitik dagilimi ---\n")
# Bunlar MC'nin n_mc=2000 ile yakalayamadigi cok kucuk olasiliklar olmali
d %>% filter(p_a > 0, p_m == 0) %>%
  summarise(n = n(),
            min = min(p_a), medyan = median(p_a),
            p95 = quantile(p_a, .95), maks = max(p_a)) %>%
  mutate(across(where(is.numeric), ~ signif(.x, 3))) %>%
  as.data.frame() %>% print(row.names = FALSE)

cat("\n--- Yalniz MC pozitif olan hucreler (beklenmez!) ---\n")
tmp <- d %>% filter(p_m > 0, p_a == 0)
if (nrow(tmp) == 0) {
  cat("Yok — beklenen durum.\n")
} else {
  tmp %>% transmute(ssp, district, year, month, temp = round(temp,1),
                    lam_a = signif(lam_a,3), lam_m = signif(lam_m,3),
                    p_m = signif(p_m,3)) %>%
    head(15) %>% as.data.frame() %>% print(row.names = FALSE)
  cat("Toplam:", nrow(tmp), "hucre. Analitik sifir verirken MC pozitif — incelenmeli.\n")
}