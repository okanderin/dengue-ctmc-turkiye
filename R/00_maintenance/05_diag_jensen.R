# R/00_maintenance/05_diag_jensen.R
library(here); library(dplyr); library(readr); library(purrr); library(tibble)

# 1) EIP onbellegi nasil kurulmus?
src <- readLines(here("R","03_models","parameter_functions.R"), warn = FALSE)
i <- grep("meanlog|rlnorm|sdlog", src)
cat("--- parameter_functions.R: lognormal kurulumu ---\n")
cat(paste0(sprintf("%4d| ", i), src[i], collapse = "\n"), "\n\n")

# 2) Ampirik kontrol: cekilen EIP ortalamasi hedef mu?
mu <- 10; sdlog <- 0.487
set.seed(1)
cat("meanlog = log(mu)        -> E[EIP] =", mean(rlnorm(2e5, log(mu), sdlog)), "\n")
cat("meanlog = log(mu)-sd^2/2 -> E[EIP] =", mean(rlnorm(2e5, log(mu) - sdlog^2/2, sdlog)), "\n")
cat("hedef mu =", mu, "\n\n")

# 3) Log-oran, sicaklik ve R0 ile nasil degisiyor?
d <- map_dfr(c("ssp126","ssp245","ssp585"), function(s) {
  a <- read_csv(here("outputs","_canonical",s,"analytic",
                     "ctmc_spark_monthly_2025_2075.csv"), show_col_types=FALSE)
  m <- read_csv(here("outputs","_canonical",s,"simulation",
                     "ctmc_spark_monthly_2025_2075_rep1000.csv"), show_col_types=FALSE)
  pa <- grep("^p_establishment", names(a), value=TRUE)[1]
  la <- grep("^lambda_local_i1", names(a), value=TRUE)[1]
  tibble(ssp = s, district = m$district_id, species = m$species,
         temp = m$temp_c,
         lam_a = a[[la]], lam_m = m$lambda_local_i1_mean,
         p_a = a[[pa]],  p_m = m$p_establishment_mean)
})

cat("--- lambda duzeyinde fark var mi? (varsa sorun EIP'te) ---\n")
d %>% filter(lam_a > 0, lam_m > 0) %>%
  mutate(lr_lam = log10(lam_a/lam_m)) %>%
  group_by(species) %>%
  summarise(n=n(), med_lam = median(lr_lam), max_lam = max(abs(lr_lam))) %>%
  as.data.frame() %>% print(row.names = FALSE)

cat("\n--- P_est log-orani: tur bazinda (sdlog turden turdur) ---\n")
d %>% filter(p_a > 0, p_m > 0) %>%
  mutate(lr = log10(p_a/p_m)) %>%
  group_by(species) %>%
  summarise(n=n(), med=median(lr), p95=quantile(abs(lr),.95)) %>%
  as.data.frame() %>% print(row.names = FALSE)

cat("\n--- Sicaklik dilimine gore ---\n")
d %>% filter(p_a > 0, p_m > 0) %>%
  mutate(lr = log10(p_a/p_m), bin = cut(temp, c(-Inf,18,22,26,30,Inf))) %>%
  group_by(bin) %>% summarise(n=n(), med=median(lr), .groups="drop") %>%
  as.data.frame() %>% print(row.names = FALSE)

cat("\n--- Sifir/NA hucre sayimi ---\n")
d %>% summarise(toplam = n(),
                p_a_sifir = sum(p_a == 0, na.rm=TRUE),
                p_m_sifir = sum(p_m == 0, na.rm=TRUE),
                ikisi_pozitif = sum(p_a > 0 & p_m > 0, na.rm=TRUE)) %>%
  as.data.frame() %>% print(row.names = FALSE)