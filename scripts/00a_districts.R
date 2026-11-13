# =============================================================================
# 00a_districts.R  —  the district layer used by every v2 step (Census 2011 boundaries)
# -----------------------------------------------------------------------------
# Written 3 Oct 2026. Why: the NFHS district shapefile used until now
# (~/Downloads/district_nfhs_shapefile/nfhs_data.shp, 642 polygons, UTM 44N) is spatially WARPED.
# Against the Census 2011 district boundaries (datameet, below; all 641 census codes match one to one)
# its polygon centroids are off by a median 10.6 km (Delhi 12-13, Bengaluru 15, Hyderabad 14, Kolkata 15,
# Chennai 28, Karaikal 33, Imphal West 79, Andaman & Nicobar 130-196 km). Chennai's and Karaikal's
# polygons lie mostly in the Bay of Bengal (zero gridded population; no NFHS clusters), and facility,
# NFHS-cluster and raster assignments near every district border were affected.
#
# What this script does: takes the Census 2011 polygons (datameet/maps, Districts/Census_2011/2011_Dist,
# kept in data/raw/datameet_census2011/ and checked by SHA-256) and attaches the NFHS layer's
# identifiers (dist_code -> zone_uid, dist_name, state_name) by census code. Identifiers, names and
# therefore every NFHS-coded join are unchanged; only the geometry is replaced.
#   - census code 0 appears twice in the NFHS layer: dist_code -102 (Jammu & Kashmir, the area
#     administered by Pakistan; no data) is matched to datameet's census code 0 "Data Not Available";
#     dist_code -100 (an unnamed polygon over the Great Rann of Kutch, no NFHS data, 0.09 M people)
#     has no counterpart because the Census 2011 layer includes the Rann in Kachchh. It is dropped,
#     so the layer has 641 districts (N_DIST in 00_config).
# Output: data/derived/districts_census2011.gpkg (EPSG:4326) and .geojson (for the app, step 22).
# v1 (the submitted paper) keeps the NFHS shapefile so that its numbers still reproduce exactly.
# =============================================================================
source(file.path(Sys.getenv("DCEJ_ROOT", unset = (function(){a<-commandArgs(FALSE);f<-sub("^--file=","",grep("^--file=",a,value=TRUE));d<-if(length(f)) dirname(normalizePath(f[1])) else getwd();while(!file.exists(file.path(d,"scripts","00_config.R"))&&d!=dirname(d)) d<-dirname(d);d})()), "scripts", "00_config.R"))
sf_use_s2(FALSE)

DM_DIR <- file.path(dir_data, "raw", "datameet_census2011")
# Pinned to commit 5ea9037 (22 May 2014), the commit whose four files match the checksums below.
DM_URL <- "https://raw.githubusercontent.com/datameet/maps/5ea90379e67dece749b01448ca438161a3534b79/Districts/Census_2011/2011_Dist.%s"
DM_SHA <- c(shp = "3636e627a519f67b4615b6a97df48c2b279e8c7b2566d3bfbc3eda1d06e33206",
            shx = "12b6028dfac9e3f20686b3d0a44f9d3989acfdb3f3cda853881d1cf2d8579eab",
            dbf = "2c7b03578fdf41a4c4e29941a564fa90bf0b0e4f865f5e54481ef7ac8494eb7d",
            prj = "a02a27b1d1982c8516d83398e85a3c8b1aef1713c13ef4d84d7bde17430c07c4")
dir.create(DM_DIR, recursive = TRUE, showWarnings = FALSE)
for (e in names(DM_SHA)) {
  f <- file.path(DM_DIR, paste0("2011_Dist.", e))
  if (!file.exists(f)) { msg("downloading 2011_Dist.%s", e); assert(system2("curl", c("-sSL", "--fail", "-o", shQuote(f), shQuote(sprintf(DM_URL, e)))) == 0, "download failed: %s", e) }
  sha <- sub(" .*", "", system2("shasum", c("-a", "256", shQuote(f)), stdout = TRUE)[1])
  assert(sha == DM_SHA[[e]], "2011_Dist.%s has SHA-256 %s, expected %s (the datameet file changed?)", e, sha, DM_SHA[[e]])
}

dm <- st_make_valid(st_read(file.path(DM_DIR, "2011_Dist.shp"), quiet = TRUE)); st_crs(dm) <- 4326
dm$cc <- as.integer(dm$censuscode)
assert(nrow(dm) == 641 && !anyDuplicated(dm$cc), "datameet layer: expected 641 polygons with unique census codes")

nf <- as.data.table(st_drop_geometry(st_read(path_districts_nfhs_shp, quiet = TRUE)))
nf <- nf[, .(dist_code = as.integer(dist_code), cc = as.integer(censuscode), dist_name = as.character(dist_name), state_name = as.character(state_name))]
assert(nrow(nf) == 642, "NFHS layer: expected 642 rows")
nf <- nf[dist_code != -100L]                                   # the Rann polygon (see header)
assert(!anyDuplicated(nf$cc) && setequal(nf$cc, dm$cc), "census codes of the NFHS layer and the Census 2011 layer do not match one to one")

d <- merge(dm[, c("cc", "DISTRICT", "ST_NM")], nf, by = "cc")
d$zone_uid <- zone_uid_of(d$dist_code)
d <- d[order(d$dist_code), c("zone_uid", "dist_code", "dist_name", "state_name", "cc", "DISTRICT", "ST_NM")]
names(d)[names(d) %in% c("cc", "DISTRICT", "ST_NM")] <- c("censuscode", "census_name", "census_state")
assert(nrow(d) == 641L, "district layer has %d rows, expected 641", nrow(d))

# name agreement between the two layers (informative: spellings differ, codes are what join)
nm <- function(x) gsub("[^a-z]", "", tolower(x))
dis <- st_drop_geometry(d)[nm(d$dist_name) != nm(d$census_name) & d$dist_code > 0, c("dist_name", "census_name")]
msg("names that differ only in spelling or renaming between the NFHS and Census 2011 layers: %d (e.g. %s)", nrow(dis),
    paste(head(paste(dis$dist_name, "/", dis$census_name), 4), collapse = "; "))

out_g <- file.path(dir_derived, "districts_census2011.gpkg"); out_j <- file.path(dir_derived, "districts_census2011.geojson")
unlink(list.files(dir_derived, pattern = "^districts_zones.*\\.rds$", full.names = TRUE))      # clear read_districts() caches first
for (f in c(out_g, out_j)) if (file.exists(f)) unlink(f)
st_write(d, out_g, quiet = TRUE)
dj <- st_make_valid(suppressWarnings(st_simplify(d, dTolerance = 0.004, preserveTopology = TRUE)))   # ~400 m, for the web app only
assert(nrow(dj) == nrow(d) && all(st_is_valid(dj)) && !any(st_is_empty(dj)), "simplified app layer lost or broke a district")
st_write(dj, out_j, quiet = TRUE)
msg("wrote %s: %d districts (Census 2011 geometry, NFHS identifiers)", basename(out_g), nrow(d))
