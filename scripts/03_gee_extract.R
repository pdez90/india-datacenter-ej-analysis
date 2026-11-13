# =============================================================================
# 03_gee_extract.R  —  Earth Engine district means with rgee
# -----------------------------------------------------------------------------
# Products and periods follow manuscript section 2.2 and SI Tables S1.2-S1.3.
# Each product is reduced to a district mean (or sum) over the district layer (read_districts():
# v2 = Census 2011 boundaries, 641 districts; v1 = the 642 NFHS polygons) and written to
# dir_gee/gee_<product>.csv (zone_uid + variables): data/gee/census2011/ for v2, data/gee/ for v1.
# 06_analysis_frame.R picks them up automatically. Polygons are simplified to ~100 m before upload
# (all products are 1-11 km rasters).
#
#   era5_heat   ERA5-Land daily: days with Tmax >= 35 C per year (2011-2025 mean)
#               -> hot_days_peryr_era5; mean 2 m temperature (C) -> t2m_mean_c
#   modis_lst   MOD11A2 daytime LST 2011-2024 mean (C)          -> lst_day_c
#   viirs_ntl   VIIRS DNB annual (V22) 2019-2024 mean          -> ntl_mean
#   ghsl_2020   GHSL P2023A population (sum), built volume (sum) -> ghsl_pop_2020, ghsl_built_volume_2020
#   worldpop    WorldPop 2020 (sum), cross-check for pop_2020  -> worldpop_2020
#   s5p_*       Sentinel-5P OFFL L3 columns 2019-2024 mean (mol/m2), reduced at 5 km
#               (native TROPOMI pixels ~5.5 x 3.5 km; the 1.1 km L3 grid oversamples them,
#               and reducing at 1.1 km made each product take ~1 h):
#               no2_column, so2_column, co_column, o3_column, hcho_column
#   jrc_gsw     JRC Global Surface Water occurrence (%)        -> gsw_occurrence
#
# ONE-TIME SETUP (in R):
#   install.packages(c("rgee", "geojsonio"))
#   rgee::ee_install()               # creates a Python env with earthengine-api
#   rgee::ee_Initialize(project = "YOUR-GCP-PROJECT")   # opens a browser to sign in
# Then:  Rscript scripts/03_gee_extract.R          (or source() it in RStudio)
#   GEE_PROJECT=my-project Rscript scripts/03_gee_extract.R
#   GEE_ONLY=s5p_so2,viirs_ntl Rscript scripts/03_gee_extract.R   (subset)
#
# Districts are sent to Earth Engine in chunks (getInfo), so no asset upload or
# Google Drive is needed. A product whose CSV already exists is skipped; delete
# the CSV to re-extract it. Long collections (TROPOMI) take several minutes.
# =============================================================================
source(file.path(Sys.getenv("DCEJ_ROOT", unset = (function(){a<-commandArgs(FALSE);f<-sub("^--file=","",grep("^--file=",a,value=TRUE));d<-if(length(f)) dirname(normalizePath(f[1])) else getwd();while(!file.exists(file.path(d,"scripts","00_config.R"))&&d!=dirname(d)) d<-dirname(d);d})()), "scripts", "00_config.R"))
# Point reticulate at rgee's own Python (set in ~/.Renviron by ee_install) BEFORE anything starts
# Python, and silence the deprecation/Future warnings rgee prints for the whole catalog at start-up.
ee_py <- Sys.getenv("EARTHENGINE_PYTHON", unset = path.expand("~/.virtualenvs/rgee/bin/python"))
Sys.setenv(RETICULATE_PYTHON = ee_py, PYTHONWARNINGS = "ignore")
reticulate::use_python(ee_py, required = TRUE)
suppressPackageStartupMessages(library(rgee))
# Earth Engine re-enables its deprecation warnings internally, so mute the printer itself
reticulate::py_run_string("import warnings; warnings.filterwarnings('ignore'); warnings.showwarning = lambda *a, **k: None")

gee_project <- Sys.getenv("GEE_PROJECT", unset = "ee-priyankadesouza")
if (gee_project != "") ee_Initialize(project = gee_project, quiet = TRUE) else ee_Initialize(quiet = TRUE)

CHUNK <- as.integer(Sys.getenv("GEE_CHUNK", unset = "25"))
only  <- strsplit(Sys.getenv("GEE_ONLY", unset = ""), ",")[[1]]

dist <- st_transform(read_districts()[, "zone_uid"], 4326)
dist <- st_make_valid(dist)
dist <- dist[!st_is_empty(dist), ]
dist <- st_make_valid(st_simplify(dist, dTolerance = 0.001, preserveTopology = TRUE))   # ~100 m; keeps the upload small

# ---- products (server-side images) ---------------------------------------------------
yrs <- function(a, b) ee$List$sequence(a, b)
products <- list(
  era5_heat = list(scale = 11132, reducer = "mean", img = function() {
    era <- ee$ImageCollection("ECMWF/ERA5_LAND/DAILY_AGGR")
    # one image per year (built in R, no server-side list callback), each the count of days with Tmax >= 35 C.
    # The map callback's argument must be named `image`: earthengine-api >= 1.x passes it by keyword.
    yearly <- lapply(2011:2025, function(y)
      era$filterDate(sprintf("%d-01-01", y), sprintf("%d-01-01", y + 1))$select("temperature_2m_max")$
        map(ee_utils_pyfunc(function(image) ee$Image(image)$gte(308.15)))$sum())
    hot <- ee$ImageCollection$fromImages(yearly)$mean()$rename("hot_days_peryr_era5")
    t2m <- era$filterDate("2011-01-01", "2026-01-01")$select("temperature_2m")$mean()$subtract(273.15)$rename("t2m_mean_c")
    hot$addBands(t2m) }),
  modis_lst = list(scale = 1000, reducer = "mean", img = function()
    ee$ImageCollection("MODIS/061/MOD11A2")$filterDate("2011-01-01", "2025-01-01")$select("LST_Day_1km")$
      mean()$multiply(0.02)$subtract(273.15)$rename("lst_day_c")),
  viirs_ntl = list(scale = 500, reducer = "mean", img = function()
    ee$ImageCollection("NOAA/VIIRS/DNB/ANNUAL_V22")$filterDate("2019-01-01", "2025-01-01")$select("average")$mean()$rename("ntl_mean")),
  ghsl_2020 = list(scale = 100, reducer = "sum", img = function()
    { # GHSL codes sea / no-data pixels with a negative value: mask them before summing
      pop <- ee$Image("JRC/GHSL/P2023A/GHS_POP/2020")$select("population_count")
      bv  <- ee$Image("JRC/GHSL/P2023A/GHS_BUILT_V/2020")$select("built_volume_total")
      pop$updateMask(pop$gte(0))$rename("ghsl_pop_2020")$addBands(bv$updateMask(bv$gte(0))$rename("ghsl_built_volume_2020")) }),
  worldpop = list(scale = 100, reducer = "sum", img = function()
    ee$ImageCollection("WorldPop/GP/100m/pop")$filter(ee$Filter$eq("country", "IND"))$filter(ee$Filter$eq("year", 2020))$mosaic()$rename("worldpop_2020")),
  s5p_no2  = list(scale = 5000, reducer = "mean", img = function() s5p("COPERNICUS/S5P/OFFL/L3_NO2", "tropospheric_NO2_column_number_density", "no2_column")),
  s5p_so2  = list(scale = 5000, reducer = "mean", img = function() s5p("COPERNICUS/S5P/OFFL/L3_SO2", "SO2_column_number_density", "so2_column")),
  s5p_co   = list(scale = 5000, reducer = "mean", img = function() s5p("COPERNICUS/S5P/OFFL/L3_CO", "CO_column_number_density", "co_column")),
  s5p_o3   = list(scale = 5000, reducer = "mean", img = function() s5p("COPERNICUS/S5P/OFFL/L3_O3", "O3_column_number_density", "o3_column")),
  s5p_hcho = list(scale = 5000, reducer = "mean", img = function() s5p("COPERNICUS/S5P/OFFL/L3_HCHO", "tropospheric_HCHO_column_number_density", "hcho_column")),
  jrc_gsw  = list(scale = 300, reducer = "mean", img = function()
    ee$Image("JRC/GSW1_4/GlobalSurfaceWater")$select("occurrence")$unmask(0)$rename("gsw_occurrence"))
)
india <- ee$Geometry$Rectangle(c(67.9, 5.9, 98.1, 37.6))
s5p <- function(id, band, out) ee$ImageCollection(id)$filterDate("2019-01-01", "2025-01-01")$filterBounds(india)$select(band)$mean()$rename(out)

# ---- extraction in chunks -------------------------------------------------------------
# Direct reduceRegions + getInfo (no R callbacks, which newer earthengine-api versions reject).
reduce_chunk <- function(img, polys, red, scale, rkind = "mean") {
  fc  <- sf_as_ee(polys[, "zone_uid"], quiet = TRUE)
  out <- img$reduceRegions(collection = fc, reducer = red, scale = scale, tileScale = 4)$getInfo()
  bands <- unlist(img$bandNames()$getInfo())
  rname <- rkind
  rbindlist(lapply(out$features, function(f) {
    pr <- f$properties
    # single-band images come back under the reducer's name ("mean"/"sum"), multi-band under band names
    get1 <- function(b) { v <- pr[[b]]; if (is.null(v) && length(bands) == 1) v <- pr[[rname]]; if (is.null(v)) NA_real_ else as.numeric(v) }
    as.data.table(c(list(zone_uid = pr$zone_uid), setNames(lapply(bands, get1), bands)))
  }))
}
extract_product <- function(name, p) {
  f <- file.path(dir_gee, sprintf("gee_%s.csv", name))
  if (file.exists(f)) { msg("%s: exists, skipped", basename(f)); return(invisible()) }
  img <- p$img(); red <- if (p$reducer == "sum") ee$Reducer$sum() else ee$Reducer$mean()
  idx <- split(seq_len(nrow(dist)), ceiling(seq_len(nrow(dist)) / CHUNK))
  res <- vector("list", length(idx))
  for (k in seq_along(idx)) {
    tries <- 0
    repeat {
      tries <- tries + 1
      r <- tryCatch(reduce_chunk(img, dist[idx[[k]], ], red, p$scale, p$reducer), error = function(e) e)
      if (!inherits(r, "error")) break
      if (tries >= 3) stop(sprintf("%s chunk %d failed 3 times: %s", name, k, conditionMessage(r)))
      msg("%s chunk %d: %s - retrying", name, k, conditionMessage(r)); Sys.sleep(10 * tries)
    }
    res[[k]] <- as.data.table(r)
    if (k %% 5 == 0 || k == length(idx)) msg("%s: %d/%d chunks", name, k, length(idx))
  }
  out <- rbindlist(res, fill = TRUE)
  assert(nrow(out) == nrow(dist) && !anyDuplicated(out$zone_uid), "%s: expected one row per district", name)
  fwrite(out, f); msg("wrote %s (%d districts)", basename(f), nrow(out))
}

todo <- if (length(only) && nzchar(only[1])) products[only] else products
for (n in names(todo)) extract_product(n, todo[[n]])

# ---- quick checks against the frozen / published values -----------------------------------
gold <- as.data.table(st_drop_geometry(st_read(file.path(dir_data, "golden", "districts.geojson"), quiet = TRUE)))
chk <- function(file, var, gvar) {
  f <- file.path(dir_gee, file); if (!file.exists(f)) return(NULL)
  m <- merge(fread(f)[, c("zone_uid", var), with = FALSE], gold[, c("zone_uid", gvar), with = FALSE], by = "zone_uid")
  data.table(variable = var, vs = gvar, r = cor(m[[var]], m[[gvar]], use = "c"), median_ratio = median(m[[var]] / m[[gvar]], na.rm = TRUE))
}
print(rbindlist(list(chk("gee_era5_heat.csv", "hot_days_peryr_era5", "hot_days_peryr"),
                     chk("gee_worldpop.csv", "worldpop_2020", "pop_2020"),
                     chk("gee_ghsl_2020.csv", "ghsl_pop_2020", "pop_2020"))))
