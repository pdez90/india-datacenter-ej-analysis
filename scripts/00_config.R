# =============================================================================
# 00_config.R  —  shared configuration for the India data-center EJ pipeline
# -----------------------------------------------------------------------------
# Rebuilt 29-30 Sep 2026 after the original pipeline was lost. Every script
# sources this file first. Paths are absolute (derived from this file's
# location), so scripts can be run from any working directory:
#     Rscript scripts/15_attribution.R            (from the project root)
#     Rscript 15_attribution.R                    (from scripts/)
#     DC_SCOPE=all Rscript scripts/15_attribution.R
# In RStudio, open the project folder and source() any script.
# =============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(sf)
})
sf_use_s2(FALSE)   # planar ops on projected/lon-lat data, as in the original

# ---- project root ------------------------------------------------------------
.this_dir <- function() {
  a <- commandArgs(FALSE)
  f <- sub("^--file=", "", grep("^--file=", a, value = TRUE))
  if (length(f)) return(dirname(normalizePath(f[1])))
  if (!is.null(sys.frames()[[1]]$ofile)) return(dirname(normalizePath(sys.frames()[[1]]$ofile)))
  getwd()
}
ROOT <- Sys.getenv("DCEJ_ROOT", unset = "")
if (ROOT == "") {
  d <- .this_dir()
  while (!file.exists(file.path(d, "scripts", "00_config.R")) && d != dirname(d)) d <- dirname(d)
  ROOT <- d
}
ROOT <- normalizePath(ROOT)

# ---- scope switch ------------------------------------------------------------
#   operating : the operating facilities (primary; every result in the paper): v2 233, v1 209
#   all       : stock plus pipeline (scenario): v2 329, v1 297
DC_SCOPE <- Sys.getenv("DC_SCOPE", unset = "operating")
stopifnot(DC_SCOPE %in% c("operating", "all"))

# ---- inventory version -----------------------------------------------------------
#   v2 : REVISED inventory (30 Sep 2026; default). Built by 00b_inventory_v2.R from v1 plus the
#        explicit, sourced edits in data/public/inventory_corrections_v2.csv and the additions in
#        inventory_additions_v2.csv (government data centres; Atlas-noticed sites, each primary-sourced).
#        233 operating facilities in 61 districts; 329 with the pipeline (66 districts).
#   v1 : the inventory of the submitted paper (frozen 1 Sep 2026; 209 / 31; 297 / 37).
#        Its file is kept only as the input that 00b_inventory_v2.R corrects; it is not run on its own.
INVENTORY <- Sys.getenv("DCEJ_INVENTORY", unset = "v2")
# Only the current inventory is supported; the 1 Sep 2026 build is no longer released or run.
if (INVENTORY != "v2") stop("Only the current inventory (v2, 30 Sep 2026) is supported.")
is_v1 <- INVENTORY == "v1"
G <- function(v1, v2) if (is_v1) v1 else v2          # expected (golden) value for the active inventory
V2_N_ALL <- 329L; V2_N_OP <- 233L                     # v2 (30 Sep 2026, with additions): facilities / operating
V2_HOST_OP <- 61L; V2_HOST_ALL <- 66L  # v2 hosting districts (site geocodes, Census 2011 boundaries; set 3 Oct 2026)

dir_data    <- file.path(ROOT, "data")
path_inventory <- file.path(dir_data, if (is_v1) "inventory_frozen_20260901.csv" else "inventory_v2_20260930.csv")
dir_derived <- file.path(dir_data, "derived")          # scope-independent intermediates
path_frame  <- file.path(dir_derived, if (is_v1) "analysis_district_v1.csv" else "analysis_district.csv")
out_dirname <- function(scope) paste0("outputs", if (is_v1) "_v1" else "", if (scope == "all") "_all" else "")
dir_out     <- file.path(ROOT, out_dirname(DC_SCOPE))
dir_fig     <- file.path(dir_out, "figures")
# publication figures: only the operating scope of the active (v2) inventory writes to paper/figs;
# the all scope and any v1 regression run write to their own figures/ folder (fix 3 Oct 2026)
dir_paperfig <- if (DC_SCOPE == "operating" && !is_v1) file.path(ROOT, "paper", "figs") else dir_fig
dir_logs    <- file.path(ROOT, "outputs", "run_logs")
for (p in c(dir_derived, dir_out, dir_fig, dir_paperfig, dir_logs)) dir.create(p, recursive = TRUE, showWarnings = FALSE)

# ---- external inputs (large files that live outside the project) -------------
# Edit these if the files move. Each can also be overridden by an env variable.
HOME <- path.expand("~")
ext <- function(var, default) { v <- Sys.getenv(var, unset = ""); if (v == "") default else path.expand(v) }
# NFHS district shapefile: source of the district identifiers (dist_code -> zone_uid, names). Its
# geometry is warped (00a_districts.R) and is used only for v1, the submitted paper's reproduction.
path_districts_nfhs_shp <- ext("DCEJ_DISTRICTS_SHP", file.path(HOME, "Downloads/district_nfhs_shapefile/nfhs_data.shp"))
path_nfhs5_pr      <- ext("DCEJ_NFHS5_PR",      file.path(HOME, "Downloads/DHS_India/India2019/IAPR7DDT/IAPR7DFL.DTA"))
path_nfhs5_gps     <- ext("DCEJ_NFHS5_GPS",     file.path(HOME, "Downloads/IAGE7AFL/IAGE7AFL.shp"))
dir_acag_sigma     <- ext("DCEJ_ACAG_SIGMA",    file.path(HOME, "Downloads/Monthly 2"))        # V5GL06.HybridPM25E (uncertainty)
dir_no2_anenberg   <- ext("DCEJ_NO2_1KM",       file.path(HOME, "Downloads/DHS_India/NO2"))    # Anenberg et al. 2022, YYYY_final_1km.tif
INMAP_VERSION      <- "v1.9.6"   # VarGridDataVersion 1.6.1, same as v1.8.0-v1.9.x
.inmap_os <- switch(Sys.info()[["sysname"]], Darwin = "darwin", Linux = "linux", Windows = "windows")
.inmap_arch <- if (grepl("arm64|aarch64", Sys.info()[["machine"]])) "arm64" else "amd64"
path_inmap_bin     <- ext("DCEJ_INMAP_BIN",     file.path(ROOT, "tools", sprintf("inmap-%s-%s-%s%s", INMAP_VERSION, .inmap_os, .inmap_arch,
                                                                              if (.inmap_os == "windows") ".exe" else "")))
dir_inmap          <- file.path(ROOT, "inmap_work")      # SHARED between scopes (16b -> 16c -> 16d)
dir_gee            <- file.path(dir_data, "gee", if (is_v1) "" else "census2011")   # 03's district CSVs (per district layer)
dir.create(dir_gee, showWarnings = FALSE, recursive = TRUE)

# ---- files shared across scripts ----------------------------------------------
siting_points_file <- "dc_corroborated_points.rds"      # written by 01 into dir_out
path_grid_params   <- file.path(dir_data, "india_grid_params.csv")

# ---- small helpers --------------------------------------------------------------
msg <- function(...) cat(sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")), sprintf(...), "\n", sep = "")
assert <- function(cond, ...) if (!isTRUE(cond)) {
  if (Sys.getenv("DCEJ_SOFT_ASSERTS") == "1") message("[check failed, continuing] ", sprintf(...)) else stop(sprintf(...), call. = FALSE) }
out  <- function(f) file.path(dir_out, f)
outp <- function(f, scope) file.path(ROOT, out_dirname(scope), f)
zone_uid_of <- function(code) sprintf("DIST_%04d", as.integer(code))

# 2015 NFHS geography has no Telangana. District-level crosswalk used everywhere
# a state label is reported (spec gotcha 10): these districts move to Telangana.
TELANGANA_DISTRICTS <- c("Adilabad", "Nizamabad", "Karimnagar", "Medak", "Hyderabad",
                         "Rangareddy", "Mahbubnagar", "Nalgonda", "Warangal", "Khammam")
state_fix <- function(state, district) fifelse(state == "Andhra Pradesh" & district %in% TELANGANA_DISTRICTS,
                                               "Telangana", state)

# The district layer, read once and cached.
#   v2: Census 2011 district boundaries with the NFHS identifiers (641 districts), built by
#       00a_districts.R (3 Oct 2026) because the NFHS shapefile's geometry is warped by ~10-30 km.
#   v1: the NFHS shapefile itself (642 polygons), so the submitted paper still reproduces.
N_DIST <- if (is_v1) 642L else 641L
path_districts <- file.path(dir_derived, "districts_census2011.gpkg")
read_districts <- function() {
  cache <- file.path(dir_derived, if (is_v1) "districts_zones_nfhs.rds" else "districts_zones_c2011.rds")
  if (file.exists(cache)) return(readRDS(cache))
  if (!is_v1) {
    assert(file.exists(path_districts), "%s not found: run scripts/00a_districts.R first", path_districts)
    d <- st_read(path_districts, quiet = TRUE)
    assert(nrow(d) == N_DIST, "district layer has %d rows, expected %d", nrow(d), N_DIST)
    d <- st_transform(d[, c("zone_uid", "dist_code", "dist_name", "state_name")], 32644)   # UTM 44N, the NFHS layer's CRS: downstream offsets/jitters are in metres
    st_geometry(d) <- "geometry"                                                            # the shapefile's column name, which scripts select by name
    saveRDS(d, cache)
    return(d)
  }
  assert(file.exists(path_districts_nfhs_shp), "District shapefile not found: %s", path_districts_nfhs_shp)
  d <- st_read(path_districts_nfhs_shp, quiet = TRUE)
  d$zone_uid   <- zone_uid_of(d$dist_code)
  d$dist_name  <- as.character(d$dist_name)
  d$state_name <- as.character(d$state_name)
  d <- d[, c("zone_uid", "dist_code", "dist_name", "state_name")]
  saveRDS(d, cache)
  d
}

msg("config: ROOT=%s  INVENTORY=%s  DC_SCOPE=%s  outputs=%s", ROOT, INVENTORY, DC_SCOPE, basename(dir_out))
