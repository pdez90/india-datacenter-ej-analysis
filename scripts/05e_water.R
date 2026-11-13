# =============================================================================
# 05e_water.R  —  WRI Aqueduct 4.0 baseline water stress, facilities and districts
# -----------------------------------------------------------------------------
# Aqueduct 4.0 (Kuzma et al. 2023), annual baseline, sub-basin (HydroBASINS
# level 6) polygons: bws_raw = withdrawals / available renewable supply,
# bwd_raw = depletion, gtd_raw = groundwater-table decline (cm/yr).
#   Facility points: the sub-basin value at each facility (the paper's unit):
#     extremely high = bws_raw >= 0.8; high or worse = bws_raw >= 0.4.
#     Current (v2, operating): 153 of 246 (62%) >= 0.8; 172 (70%) >= 0.4.
#   Districts: area-weighted mean of sub-basin values (policy table, maps).
#     Checked against the frozen bws_raw layer.
#
# DATA: the script downloads Aqueduct 4.0 (~400 MB zip) into data/raw/ on
# first run. If the download fails, get "Aqueduct 4.0 Current and Future Global
# Maps Data" from https://www.wri.org/data/aqueduct-global-maps-40-data , unzip
# it anywhere, and set DCEJ_AQUEDUCT_GDB to the Aqueduct40_waterrisk_download_*.gdb
# (or its baseline_annual layer as .gpkg/.shp).
# =============================================================================
source(file.path(Sys.getenv("DCEJ_ROOT", unset = (function(){a<-commandArgs(FALSE);f<-sub("^--file=","",grep("^--file=",a,value=TRUE));d<-if(length(f)) dirname(normalizePath(f[1])) else getwd();while(!file.exists(file.path(d,"scripts","00_config.R"))&&d!=dirname(d)) d<-dirname(d);d})()), "scripts", "00_config.R"))

aq_path <- ext("DCEJ_AQUEDUCT_GDB", "")
if (aq_path == "") {
  cand <- list.files(file.path(dir_data, "raw"), pattern = "\\.gdb$", recursive = TRUE, include.dirs = TRUE, full.names = TRUE)
  cand <- cand[grepl("Aqueduct40", cand, ignore.case = TRUE)]
  if (!length(cand)) {
    z <- file.path(dir_data, "raw", "aqueduct40.zip")
    if (!file.exists(z)) {
      msg("downloading Aqueduct 4.0 (large; a few minutes)")
      options(timeout = 3600)
      download.file("https://files.wri.org/aqueduct/aqueduct-4-0-water-risk-data.zip", z, mode = "wb")
    }
    unzip(z, exdir = file.path(dir_data, "raw", "aqueduct40"))
    cand <- list.files(file.path(dir_data, "raw", "aqueduct40"), pattern = "\\.gdb$", recursive = TRUE, include.dirs = TRUE, full.names = TRUE)
  }
  assert(length(cand) >= 1, "Aqueduct 4.0 geodatabase not found; see the header of this script")
  aq_path <- cand[1]
}
lyr <- if (grepl("\\.gdb$", aq_path)) { l <- st_layers(aq_path)$name; l[grepl("baseline_annual", l)][1] } else NULL
msg("Aqueduct: %s%s", basename(aq_path), if (!is.null(lyr)) paste0(" [", lyr, "]") else "")
aq <- if (is.null(lyr)) st_read(aq_path, quiet = TRUE) else st_read(aq_path, layer = lyr, quiet = TRUE)
# lower-case the attribute names only (renaming the geometry column breaks the sf object)
att <- st_drop_geometry(aq); names(att) <- tolower(names(att))
keep <- intersect(c("pfaf_id", "bws_raw", "bws_cat", "bwd_raw", "gtd_raw"), names(att))
aq <- st_sf(att[, keep, drop = FALSE], geometry = st_geometry(aq))
aq <- st_transform(aq, 4326)
bb <- st_as_sfc(st_bbox(c(xmin = 67, ymin = 5, xmax = 99, ymax = 38), crs = st_crs(4326)))
aq <- aq[lengths(st_intersects(aq, bb)) > 0, ]          # sub-basins touching India's box (cheap filter first)
aq <- suppressWarnings(st_collection_extract(st_make_valid(aq), "POLYGON"))
msg("Aqueduct sub-basins in the India box: %d", nrow(aq))
for (v in c("bws_raw", "bwd_raw", "gtd_raw")) if (v %in% names(aq)) aq[[v]][aq[[v]] < -900] <- NA   # Aqueduct no-data codes

# ---- facility points (both scopes) --------------------------------------------------
for (sc in c("operating", "all")) {
  pts <- readRDS(outp(siting_points_file, sc))
  j <- st_join(st_transform(pts[, "dc_id"], 4326), aq, join = st_intersects, left = TRUE)
  j <- as.data.table(st_drop_geometry(j))[!duplicated(dc_id)]
  if (any(is.na(j$bws_raw))) {         # coastal points outside every sub-basin -> nearest sub-basin
    miss <- j[is.na(bws_raw), dc_id]
    nn <- st_nearest_feature(st_transform(pts[pts$dc_id %in% miss, ], 4326), aq)
    j[match(miss, dc_id), c(keep) := as.data.table(st_drop_geometry(aq[nn, keep]))]
  }
  j[, `:=`(bws_ext_high = bws_raw >= 0.8, bws_high_plus = bws_raw >= 0.4)]
  fwrite(j, outp("tab05e_water_facility.csv", sc))
  msg("%s: %d of %d facilities (%.0f%%) in extremely high water stress (>= 0.8); %d (%.0f%%) high or worse",
      sc, j[bws_ext_high == TRUE, .N], nrow(j), 100 * j[, mean(bws_ext_high)], j[bws_high_plus == TRUE, .N], 100 * j[, mean(bws_high_plus)])
}

# ---- districts: area-weighted mean -----------------------------------------------------------------
dist <- st_transform(read_districts()[, "zone_uid"], 4326)
ix <- suppressWarnings(st_intersection(dist, aq))
ix$area <- as.numeric(st_area(ix))
ixd <- as.data.table(st_drop_geometry(ix))
wmean <- function(x, w) { k <- !is.na(x); if (!any(k)) NA_real_ else sum(x[k] * w[k]) / sum(w[k]) }
dw <- ixd[, .(bws_raw_aq = wmean(bws_raw, area), bwd_raw = wmean(bwd_raw, area), gtd_raw = wmean(gtd_raw, area)), by = zone_uid]
fwrite(dw, file.path(dir_derived, "aqueduct_district.csv"))

gpath <- file.path(dir_data, "golden", "districts.geojson")
if (file.exists(gpath)) {
gold <- as.data.table(st_drop_geometry(st_read(gpath, quiet = TRUE)))
m <- merge(dw, gold[, .(zone_uid, bws_raw)], by = "zone_uid")
msg("district BWS vs frozen layer: r = %.4f, median |diff| = %.3f", cor(m$bws_raw_aq, m$bws_raw, use = "c"), median(abs(m$bws_raw_aq - m$bws_raw), na.rm = TRUE))
}
