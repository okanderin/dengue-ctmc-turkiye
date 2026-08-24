# =============================================================================
# 02c_freeze_remaining.R
# Rmd'nin ihtiyac duydugu ama 02'de kapsanmayan artefaktlari dondurur:
# figures, cross_scenario, validation, diagnostics, rainfall_CSI, rds ikizleri
# =============================================================================
library(here); library(dplyr); library(readr); library(digest); library(purrr)

CANON <- here("outputs", "_canonical")
SSPS  <- c("ssp126", "ssp245", "ssp585")

# MANIFEST salt-okunur olabilir
if (.Platform$OS.type == "windows")
  invisible(suppressWarnings(system2("attrib",
                                     c("-R", shQuote(file.path(CANON, "*.*")), "/S"), stdout = FALSE, stderr = FALSE)))

miss <- character()
copy_one <- function(from, to, optional = FALSE) {
  if (!file.exists(from)) {
    if (!optional) miss <<- c(miss, from)
    return(NULL)
  }
  dir.create(dirname(to), recursive = TRUE, showWarnings = FALSE)
  if (!file.copy(from, to, overwrite = TRUE)) { miss <<- c(miss, paste("FAIL:", to)); return(NULL) }
  tibble(rel_path = sub(paste0("^", CANON, "/?"), "", to), source = from,
         bytes = file.info(to)$size, sha256 = digest(file = to, algo = "sha256"))
}
new <- list(); add <- function(x) if (!is.null(x)) new[[length(new)+1]] <<- x

# --- Klasorun tamamini kopyala (png/csv/rds) ---
copy_dir <- function(from_dir, to_dir, pattern = "\\.(png|csv|rds|pdf)$") {
  if (!dir.exists(from_dir)) { miss <<- c(miss, paste("DIR YOK:", from_dir)); return(invisible()) }
  fs <- list.files(from_dir, pattern = pattern, full.names = TRUE)
  for (f in fs) add(copy_one(f, file.path(to_dir, basename(f))))
}

for (ssp in SSPS) {
  copy_dir(here("outputs", ssp, "figures"),                    file.path(CANON, ssp, "figures"))
  copy_dir(here("outputs", ssp, "validation"),                 file.path(CANON, ssp, "validation"))
  copy_dir(here("outputs", ssp, "diagnostics", "mc_validation"),
           file.path(CANON, ssp, "diagnostics", "mc_validation"))
  copy_dir(here("outputs", ssp, "rainfall_CSI"),               file.path(CANON, ssp, "rainfall_CSI"))
  
  # Rmd .rds okuyor — CSV ikizlerinin yanina rds'leri de al
  for (f in c("ctmc_spark_horizon_2025_2075_rep1000.rds",
              "ctmc_spark_yearly_2025_2075_rep1000.rds",
              "ctmc_spark_monthly_2025_2075_rep1000.rds"))
    add(copy_one(here("outputs", ssp, "simulation", f),
                 file.path(CANON, ssp, "simulation", f)))
  
  for (f in c("ctmc_spark_horizon_2025_2075.rds",
              "ctmc_spark_yearly_2025_2075.rds",
              "ctmc_spark_monthly_2025_2075.rds"))
    add(copy_one(here("outputs", ssp, "model_results", f),
                 file.path(CANON, ssp, "analytic", f)))
  
  for (ssp in SSPS)
    add(copy_one(here("outputs", ssp, "sensitivity", "importation",
                      sprintf("sensitivity_summary_%s.csv", ssp)),
                 file.path(CANON, ssp, "importation",
                           sprintf("sensitivity_summary_%s.csv", ssp))))
}

# Cross-scenario figurleri
copy_dir(here("outputs", "cross_scenario"), file.path(CANON, "cross_scenario"))

# Kok seviyesindeki konvekslik figurleri
for (f in c("convexity_EIP_curve.png", "convexity_EIP_map.png"))
  add(copy_one(here("outputs", f), file.path(CANON, f)))

# --- Manifesti guncelle ---
man <- read_csv(file.path(CANON, "MANIFEST.csv"), show_col_types = FALSE)
newm <- bind_rows(new) %>% mutate(frozen_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"))
out <- bind_rows(man, newm) %>% distinct(rel_path, .keep_all = TRUE)
write_csv(out, file.path(CANON, "MANIFEST.csv"))

cat("Eklendi:", nrow(newm), "| Toplam:", nrow(out), "\n\n")
out %>% mutate(d = dirname(rel_path)) %>% count(d) %>% as.data.frame() %>% print(row.names = FALSE)
if (length(miss)) { cat("\n!! EKSIK (", length(miss), "):\n", sep=""); cat(paste0("   ", miss, collapse="\n"), "\n") }