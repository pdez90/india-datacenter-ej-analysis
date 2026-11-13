# =============================================================================
# 04_env_local.R  —  district environmental layers built from local files
# -----------------------------------------------------------------------------
#   pm25_acag_local   ACAG V5GL06 HybridPM25 monthly 0.01 deg, 2013-2022 mean
#                     (van Donkelaar et al. 2021), exact area-weighted district
#                     mean (exactextractr). Reproduces the published layer to
#                     <3e-4 ug/m3 (cor 1.000) - validated 30 Sep 2026.
#   pm25_acag_unc     ACAG V5GL06 HybridPM25E (GWRPM25SIGMA), 2013-2022 mean;
#                     used for inverse-squared-uncertainty weights.
#   no2_surf          Anenberg et al. (2022) 1 km annual surface NO2 (ppb),
#                     2010-2020 mean.
#   coal_mw, fossil_mw, gas_mw, dist_fossil_km
#                     WRI Global Power Plant Database v1.3 (India: 253 coal,
#                     68 gas, 17 oil). Plants outside every polygon (31; coastal
#                     and border) go to the nearest district. coal_mw
#                     reproduces the published layer exactly (642/642; 131 coal
#                     districts). dist_fossil_km = distance from the district
#                     centroid (UTM 44N) to the nearest coal/gas/oil plant;
#                     the published values came from an unrecorded variant and
#                     agree to r = 0.9999 (max diff ~18 km).
# Writes data/derived/env_local_district.csv (+ plants_india.csv).
# Rasters are cropped to India and cached in data/derived/ on first run.
# =============================================================================
source(file.path(Sys.getenv("DCEJ_ROOT", unset = (function(){a<-commandArgs(FALSE);f<-sub("^--file=","",grep("^--file=",a,value=TRUE));d<-if(length(f)) dirname(normalizePath(f[1])) else getwd();while(!file.exists(file.path(d,"scripts","00_config.R"))&&d!=dirname(d)) d<-dirname(d);d})()), "scripts", "00_config.R"))
suppressPackageStartupMessages({ library(terra); library(exactextractr) })

dir_acag_pm <- ext("DCEJ_ACAG_PM", file.path(HOME, "Downloads/Monthly_PM_Global"))   # V5GL06.HybridPM25.Global.YYYYMM-YYYYMM.nc
INDIA <- terra::ext(67.9, 98.1, 5.9, 37.6)
dist  <- read_districts()
d4    <- st_transform(dist, 4326)

# ---- multi-year mean raster, cropped to India, cached -----------------------------------
mean_raster <- function(files, cache, varname = NULL) {
  if (file.exists(cache)) return(rast(cache))
  miss <- files[!file.exists(files)]
  assert(!length(miss), "%d input files missing, e.g. %s", length(miss), miss[1])
  msg("averaging %d files -> %s", length(files), basename(cache))
  s <- NULL; n <- NULL
  for (f in files) {
    r <- if (is.null(varname)) rast(f) else rast(f, subds = varname)
    if (is.na(crs(r)) || crs(r) == "") crs(r) <- "EPSG:4326"
    r <- crop(r, INDIA, snap = "near")
    r <- classify(r, cbind(-Inf, -900, NA))
    ok <- !is.na(r)
    if (is.null(s)) { s <- ifel(ok, r, 0); n <- ok * 1 } else { s <- s + ifel(ok, r, 0); n <- n + ok }
  }
  m <- ifel(n > 0, s / n, NA)
  writeRaster(m, cache, overwrite = TRUE, gdal = "COMPRESS=DEFLATE")
  rast(cache)
}
yrs_m <- function(y0, y1) unlist(lapply(y0:y1, function(y) sprintf("%d%02d", y, 1:12)))

acag  <- mean_raster(sprintf("%s/V5GL06.HybridPM25.Global.%s-%s.nc", dir_acag_pm, yrs_m(2013, 2022), yrs_m(2013, 2022)),
                     file.path(dir_derived, "acag_v5gl06_pm25_2013_2022_mean.tif"), "GWRPM25")
acagU <- mean_raster(sprintf("%s/V5GL06.HybridPM25E.Global.%s-%s.nc", dir_acag_sigma, yrs_m(2013, 2022), yrs_m(2013, 2022)),
                     file.path(dir_derived, "acag_v5gl06_sigma_2013_2022_mean.tif"), "GWRPM25SIGMA")
no2   <- mean_raster(sprintf("%s/%d_final_1km.tif", dir_no2_anenberg, 2010:2020),
                     file.path(dir_derived, "no2_anenberg_2010_2020_mean.tif"))

env <- data.table(zone_uid = dist$zone_uid,
                  pm25_acag_local = exact_extract(acag,  d4, "mean", progress = FALSE),
                  pm25_acag_unc   = exact_extract(acagU, d4, "mean", progress = FALSE),
                  no2_surf        = exact_extract(no2,   d4, "mean", progress = FALSE))

# ---- power plants (WRI GPPD v1.3) ---------------------------------------------------
gppd_file <- file.path(dir_data, "raw", "gppd.csv")
if (!file.exists(gppd_file)) {
  dir.create(dirname(gppd_file), showWarnings = FALSE, recursive = TRUE)
  download.file("https://raw.githubusercontent.com/wri/global-power-plant-database/master/output_database/global_power_plant_database.csv",
                gppd_file, mode = "wb")
}
g <- fread(gppd_file)[country == "IND" & primary_fuel %in% c("Coal", "Gas", "Oil") & capacity_mw > 0]
assert(g[primary_fuel == "Coal", .N] == 253 && abs(g[primary_fuel == "Coal", sum(capacity_mw)] - 204919.22) < 0.1,
       "GPPD India coal should be 253 plants / 204,919 MW")
# documented GPPD coordinate corrections (data/public/plant_corrections.csv; same file and rule as 16b), so the
# district coal layer used by the siting comparisons agrees with the plant layer used for the InMAP emissions (30 Oct 2026)
if (!is_v1) {
  pc <- fread(file.path(dir_data, "public", "plant_corrections.csv"), colClasses = "character")
  for (i in seq_len(nrow(pc))) {
    e <- pc[i]; r <- which(g$gppd_idnr == e$gppd_idnr)
    assert(length(r) == 1, "plant correction: gppd_idnr %s matches %d rows", e$gppd_idnr, length(r))
    assert(abs(as.numeric(g[[e$field]][r]) - as.numeric(e$old_value)) < 1e-9, "plant correction %s %s: old value not found", e$gppd_idnr, e$field)
    set(g, r, e$field, as.numeric(e$new_value))
  }
  msg("GPPD coordinate corrections applied: %d field(s) on %d plant(s) (%s)", nrow(pc), uniqueN(pc$gppd_idnr), paste(unique(pc$name), collapse = ", "))
}
p  <- st_transform(st_as_sf(g, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE), st_crs(dist))
w  <- st_within(p, dist); idx <- vapply(w, function(i) if (length(i)) i[1] else NA_integer_, 1L)
msg("plants in no polygon -> nearest district: %d", sum(is.na(idx)))
idx[is.na(idx)] <- st_nearest_feature(p[is.na(idx), ], dist)
g[, `:=`(zone_uid = dist$zone_uid[idx], plant_state = state_fix(dist$state_name[idx], dist$dist_name[idx]))]
fwrite(g[, .(gppd_idnr, name, primary_fuel, capacity_mw, latitude, longitude, zone_uid, plant_state, commissioning_year)],
       file.path(dir_derived, "plants_india.csv"))

cap <- dcast(g[, .(mw = sum(capacity_mw)), by = .(zone_uid, primary_fuel)], zone_uid ~ primary_fuel, value.var = "mw", fill = 0)
setnames(cap, c("Coal", "Gas", "Oil"), c("coal_mw", "gas_mw", "oil_mw"), skip_absent = TRUE)
env <- merge(env, cap, by = "zone_uid", all.x = TRUE)
for (v in c("coal_mw", "gas_mw", "oil_mw")) env[is.na(get(v)), (v) := 0]
env[, fossil_mw := coal_mw + gas_mw + oil_mw]
cen <- st_centroid(st_geometry(dist))
env <- merge(env, data.table(zone_uid = dist$zone_uid,
                             dist_fossil_km = apply(st_distance(cen, p), 1, min) / 1000), by = "zone_uid")
msg("coal districts: %d; >= 1 GW: %d; district-assigned coal %.0f MW", env[coal_mw > 0, .N], env[coal_mw >= 1000, .N], env[, sum(coal_mw)])


# ---- check against the frozen app layer -------------------------------------------------
golden_f <- file.path(dir_data, "golden", "districts.geojson")   # authors' frozen reference layer (not released)
if (!file.exists(golden_f)) msg("skipping the check against the frozen layer: %s not found", golden_f) else {
gold <- as.data.table(st_drop_geometry(st_read(golden_f, quiet = TRUE)))
m <- merge(env, gold[, .(zone_uid, g_pm = pm25_acag_local, g_no2 = no2_surf, g_coal = coal_mw, g_df = dist_fossil_km)], by = "zone_uid")
chk <- data.table(variable = c("pm25_acag_local", "no2_surf", "coal_mw", "dist_fossil_km"),
  max_abs_diff = c(max(abs(m$pm25_acag_local - m$g_pm), na.rm = TRUE), max(abs(m$no2_surf - m$g_no2), na.rm = TRUE),
                   max(abs(m$coal_mw - m$g_coal), na.rm = TRUE), max(abs(m$dist_fossil_km - m$g_df), na.rm = TRUE)),
  cor = c(cor(m$pm25_acag_local, m$g_pm, use = "c"), cor(m$no2_surf, m$g_no2, use = "c"),
          cor(m$coal_mw, m$g_coal, use = "c"), cor(m$dist_fossil_km, m$g_df, use = "c")))
print(chk); fwrite(chk, file.path(dir_derived, "check04_vs_frozen.csv"))
if (is_v1) {   # same polygons as the frozen layer: must reproduce it exactly
  assert(chk[variable == "coal_mw", max_abs_diff] < 1e-6, "coal capacity no longer reproduces the frozen layer")
  assert(chk[variable == "pm25_acag_local", max_abs_diff] < 0.01, "ACAG PM2.5 no longer reproduces the frozen layer")
  assert(chk[variable == "no2_surf", cor] > 0.9999, "surface NO2 no longer reproduces the frozen layer (r = %.4f): check the Anenberg files / cache",
         chk[variable == "no2_surf", cor])
} else {      # v2: Census 2011 polygons vs the frozen layer's warped NFHS polygons (10-30 km apart), so values
              # move near every border; the smooth ACAG field must still agree closely (r > 0.95), the 1 km NO2
              # field less so (observed r 0.93 on 3 Oct 2026; a wrong input gives r far below 0.8)
  assert(chk[variable == "pm25_acag_local", cor] > 0.95, "ACAG district means disagree with the frozen layer (r < 0.95): check inputs")
  assert(chk[variable == "no2_surf", cor] > 0.8, "surface NO2 district means disagree with the frozen layer (r < 0.8): check the Anenberg files / cache")
}
}   # end of the check against the frozen layer
# written only after the checks pass, so a bad input can never overwrite a good layer
fwrite(env, file.path(dir_derived, "env_local_district.csv"))
