#source("R/01_setup/init.R")
# # -------------------------------------------------------------------
# init.R
# Project initializer for dengue-ctmc-turkiye
# Guarantees that DIR_* objects are visible in analysis scripts
# -------------------------------------------------------------------


.source_safe <- function(path, envir) {
  if (!file.exists(path)) {
    stop("Missing init dependency: ", path, call. = FALSE)
  }
  source(path, local = envir)
}

# --- Require here
if (!requireNamespace("here", quietly = TRUE)) {
  stop(
    "Package 'here' is required. Install once: install.packages('here')",
    call. = FALSE
  )
}

# --- Determine and validate ROOT
# Validate the repository by stable marker files, not by the local directory
# name. This lets a normal `git clone` into `dengue-ctmc-turkiye/` work.
ROOT <- normalizePath(here::here(), winslash = "/", mustWork = TRUE)

root_markers <- c(
  "r_project_tez.Rproj",
  file.path("R", "01_setup", "packages.R"),
  file.path("R", "01_setup", "paths.R")
)
missing_markers <- root_markers[!file.exists(file.path(ROOT, root_markers))]
if (length(missing_markers) > 0L) {
  stop(
    "Project root validation failed. Missing: ",
    paste(missing_markers, collapse = ", "),
    "\nDetected ROOT: ", ROOT,
    call. = FALSE
  )
}

SETUP_DIR <- file.path(ROOT, "R", "01_setup")
if (!dir.exists(SETUP_DIR)) {
  stop("Setup directory not found: ", SETUP_DIR, call. = FALSE)
}

# 🔑 CRITICAL FIX:
# Always source setup scripts into the GLOBAL environment
TARGET_ENV <- globalenv()

.source_safe(file.path(SETUP_DIR, "packages.R"),       envir = TARGET_ENV)
.source_safe(file.path(SETUP_DIR, "paths.R"),          envir = TARGET_ENV)
.source_safe(file.path(SETUP_DIR, "global_options.R"), envir = TARGET_ENV)
