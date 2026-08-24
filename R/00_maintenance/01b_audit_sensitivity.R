# R/00_maintenance/01b_audit_sensitivity.R
library(here); library(dplyr); library(readr); library(purrr); library(digest)

SSPS <- c("ssp126","ssp245","ssp585"); KARTAL <- "TUR.40.25_1"

files <- map_dfr(SSPS, function(ssp) {
  roots <- c(here("outputs", ssp, "sensitivity", "ctmc_mc"),
             here("outputs", ssp, "sensitivity", "ctmc_mc_rep1000"))
  f <- unlist(lapply(roots[dir.exists(roots)], list.files,
                     pattern = "horizon_2025_2075_rep1000\\.csv$",
                     recursive = TRUE, full.names = TRUE))
  if (length(f) == 0) return(tibble())
  tibble(ssp = ssp, path = f)
})

res <- files %>% rowwise() %>% mutate({
  d <- read_csv(path, show_col_types = FALSE)
  col <- if ("p_ge1_major_mean" %in% names(d)) "p_ge1_major_mean" else "p_ge1_major"
  kp <- d[[col]][d$district_id == KARTAL]
  tibble(tree     = if (grepl("ctmc_mc_rep1000", path)) "rep1000" else "ctmc_mc",
         scenario = basename(dirname(path)),
         kartal_p = if (length(kp) == 1) kp else NA_real_,
         mtime    = file.info(path)$mtime)
}) %>% ungroup() %>%
  mutate(verdict = case_when(scenario != "base" & !scenario %in% c("m_100") ~ "N/A (m!=1)",
                             kartal_p > 0.90 ~ "POISSON",
                             kartal_p < 0.60 ~ "BERNOULLI",
                             TRUE ~ "?"))

res %>% select(ssp, tree, scenario, kartal_p, verdict, mtime) %>%
  arrange(ssp, tree, scenario) %>% as.data.frame() %>% print(row.names = FALSE)

cat("\n--- Agac bazinda en yeni tarih ---\n")
res %>% group_by(ssp, tree) %>% summarise(latest = max(mtime), n = n(), .groups="drop") %>%
  as.data.frame() %>% print(row.names = FALSE)