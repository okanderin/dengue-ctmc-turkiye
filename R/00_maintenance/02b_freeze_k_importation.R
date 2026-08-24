# R/00_maintenance/02b_freeze_k_importation.R
library(here); library(dplyr); library(readr); library(digest); library(tibble)

CANON <- here("outputs", "_canonical")
extra <- list()

for (ssp in c("ssp126", "ssp245", "ssp585")) {
  for (sc in c("k010", "k013", "k020")) {
    src <- here("outputs", ssp, "sensitivity", "importation", sc,
                "importation_pressure_monthly_2025_2075.rds")
    if (!file.exists(src)) { message("YOK: ", src); next }
    
    d <- readRDS(src)
    dst <- file.path(CANON, ssp, "importation", sc,
                     "importation_pressure_monthly_2025_2075.csv")
    dir.create(dirname(dst), recursive = TRUE, showWarnings = FALSE)
    write_csv(d, dst)
    
    extra[[length(extra) + 1]] <- tibble(
      rel_path  = sub(paste0("^", CANON, "/?"), "", dst),
      source    = paste(src, "(rds -> csv)"),
      bytes     = file.info(dst)$size,
      sha256    = digest(file = dst, algo = "sha256"),
      frozen_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
    )
  }
}

man <- read_csv(file.path(CANON, "MANIFEST.csv"), show_col_types = FALSE)
new <- bind_rows(man, bind_rows(extra))
write_csv(new, file.path(CANON, "MANIFEST.csv"))

cat("Eklendi:", length(extra), "| Toplam:", nrow(new), "\n")
new %>% mutate(dir = dirname(rel_path)) %>%
  filter(grepl("importation", dir)) %>% count(dir) %>%
  as.data.frame() %>% print(row.names = FALSE)