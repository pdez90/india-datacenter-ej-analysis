# =============================================================================
# 01_inventory.R  —  the facility inventory (siting points + footprint rows)
# -----------------------------------------------------------------------------
# REBUILD NOTE. The original chain that produced the inventory (ATLAS/OSM
# ingestion -> operator census -> DataCenterMap page facts -> 01d corroboration)
# was lost on 29 Sep 2026, together with its hand-curated inputs
# (operator_facilities.csv, datacentermap_india.csv + HTML cache, review and
# identity-screen decision files). DataCenterMap content of 31 Aug 2026 cannot
# be re-fetched identically.
#
# The inventory that every published number rests on survives as the companion
# app's facilities layer (github.com/pdez90/india-datacentre-ej, data/
# facilities.geojson, baked 2 Sep 2026 from the 1 Sep 2026 pipeline). It was
# frozen to data/inventory_frozen_20260901.csv: one row per facility with name,
# operator, status, primary source (census | dcm), type, reported IT load
# (current and build-out), the no-derived-MW flag of the identity screen, and
# the geocoded point. This script rebuilds everything downstream from it.
#
# Site geocodes (v2, 3 Oct 2026): the frozen inventory placed most facilities on a city centroid
# (66 distinct points). v2 replaces every point with a site geocode from
# data/facility_geocodes_v2.csv (address, address source, method and precision: building / campus /
# locality / city; 30 of the 329 rows keep a city placeholder). geocode_class carries that precision.
# District assignment (v2): point-in-polygon on the Census 2011 district boundaries (00a_districts.R).
# District assignment (v1): point-in-polygon on the 2015 NFHS districts; points in
# no polygon (the Kochi city centroid) take the nearest district, as in 01d.
# Hard-stop identities are re-asserted for the active inventory (00_config INVENTORY):
#   v1 (submitted paper): 209 operating in 31 districts; 297 in 37; 61 census + 148 DCM operating.
#   v2 (default; final inventory): 233 operating in 61 districts; 329 in 66;
#       operating sources 62 census + 123 DCM + 16 cross-check + 32 government census.
# v2 note: a site geocode that falls in no polygon takes the nearest Census 2011 polygon, which
# must lie within 3 km (otherwise the run stops); none occurs in the final inventory.
# =============================================================================
source(file.path(Sys.getenv("DCEJ_ROOT", unset = (function(){a<-commandArgs(FALSE);f<-sub("^--file=","",grep("^--file=",a,value=TRUE));d<-if(length(f)) dirname(normalizePath(f[1])) else getwd();while(!file.exists(file.path(d,"scripts","00_config.R"))&&d!=dirname(d)) d<-dirname(d);d})()), "scripts", "00_config.R"))

inv <- fread(path_inventory)
if (!"facility_id" %in% names(inv)) inv[, facility_id := sprintf("IN%03d", .I)]   # v1: ids = row order
assert(nrow(inv) == G(297, V2_N_ALL), "inventory should have %d rows, has %d", G(297, V2_N_ALL), nrow(inv))
assert(all(inv$status %in% c("operating", "construction", "announced")), "unknown status values")
assert(all(inv$src %in% c("census", "dcm", "crosscheck", "government")), "src must be census, dcm, crosscheck or government")
assert(all(inv$dc_type %in% c("hyperscale", "colocation", "telecom", "enterprise", "government")), "unknown dc_type")
if (!is_v1) {
  gc <- fread(file.path(dir_data, "facility_geocodes_v2.csv"))
  assert(nrow(gc) == nrow(inv) && setequal(gc$facility_id, inv$facility_id), "facility_geocodes_v2.csv must have one row per facility")
  assert(all(gc$geocode_precision %in% c("building", "campus", "locality", "city")), "unknown geocode_precision")
  assert(all(between(gc$lon, 68, 98) & between(gc$lat, 6, 37.5)), "geocode outside India")
  inv <- merge(inv, gc[, .(facility_id, lon_site = lon, lat_site = lat, geocode_precision)], by = "facility_id")
  inv[, moved_km := sqrt(((lon_site - lon) * 111.32 * cos(lat * pi / 180))^2 + ((lat_site - lat) * 110.57)^2)]
  msg("site geocodes: %s; median move from the frozen point %.1f km",
      paste(names(table(inv$geocode_precision)), table(inv$geocode_precision), collapse = ", "), median(inv$moved_km))
  inv[, `:=`(lon = lon_site, lat = lat_site, lon_site = NULL, lat_site = NULL, moved_km = NULL)]
}
assert(!anyDuplicated(inv[, paste(name, operator, lon, lat)]), "duplicate facility rows")

# ---- district assignment ---------------------------------------------------------
dist <- read_districts()
pts  <- st_transform(st_as_sf(inv, coords = c("lon", "lat"), crs = 4326, remove = FALSE), st_crs(dist))
w    <- st_within(pts, dist)
idx  <- vapply(w, function(i) if (length(i)) i[1] else NA_integer_, 1L)
miss <- which(is.na(idx))
if (length(miss)) {
  idx[miss] <- st_nearest_feature(pts[miss, ], dist)
  if (is_v1) {   # v1: the frozen inventory's recorded district (Kochi rows outside the generalised coastline)
    j <- match(inv$dist_name_frozen[miss], dist$dist_name)
    idx[miss] <- ifelse(is.na(j), idx[miss], j)
  } else {       # v2: nearest Census 2011 polygon, which must be close (a site geocode is never far offshore)
    dm <- as.numeric(st_distance(pts[miss, ], dist[idx[miss], ], by_element = TRUE))
    assert(all(dm < 3000), "site geocode > 3 km from every district polygon: %s", paste(inv$facility_id[miss][dm >= 3000], collapse = ", "))
  }
  msg("%d facilities in no polygon -> %s (%s)", length(miss), if (is_v1) "recorded administrative district" else "nearest district (< 3 km)",
      paste(unique(dist$dist_name[idx[miss]]), collapse = ", "))
}
if (!is_v1) {   # documented administrative district where the generalised boundary splits an address (2 rows)
  ov <- gc[nzchar(district_override), .(facility_id, district_override)]
  k <- match(ov$facility_id, inv$facility_id); j <- match(ov$district_override, dist$dist_name)
  assert(!anyNA(j), "district_override not a district name: %s", paste(ov$district_override[is.na(j)], collapse = ", "))
  assert(all(ov$district_override %in% dist$dist_name[!duplicated(dist$dist_name) & !dist$dist_name %in% dist$dist_name[duplicated(dist$dist_name)]]),
         "district_override must be a district name that is unique across states")
  idx[k] <- j; msg("district overrides from facility_geocodes_v2.csv: %s", paste(ov$facility_id, ov$district_override, collapse = "; "))
}
inv[, `:=`(zone_uid = dist$zone_uid[idx], dist_name = dist$dist_name[idx],
           state_name = state_fix(dist$state_name[idx], dist$dist_name[idx]))]
if (is_v1) {
  assert(all(inv$dist_name == inv$dist_name_frozen),
         "district assignment differs from the frozen inventory for %d rows", sum(inv$dist_name != inv$dist_name_frozen))
} else {   # record every facility whose district or state differs from the frozen (warped-map, city-centroid) one
  ch <- inv[dist_name != dist_name_frozen | state_name != state_fix(state_name_frozen, dist_name_frozen),
            .(facility_id, name, status, district_frozen = dist_name_frozen, state_frozen = state_fix(state_name_frozen, dist_name_frozen),
              district = dist_name, state = state_name, geocode_precision)]
  fwrite(ch[order(facility_id)], file.path(dir_data, "public", "facility_district_changes_v2.csv"))
  msg("district changed for %d facilities (%d operating), state for %d (%d operating): data/public/facility_district_changes_v2.csv",
      nrow(ch), ch[status == "operating", .N], ch[state != state_frozen, .N], ch[status == "operating" & state != state_frozen, .N])
}

# ---- stable ids and scope ----------------------------------------------------------
setorder(inv, status, dist_name, operator, name)
inv[, dc_id := sprintf("DC%03d", .I)]
inv[, is_operating := status == "operating"]
inv[, geocode_class := if (is_v1) fifelse(src == "census", "census_city", "dcm_city") else geocode_precision]

# ---- hard-stop provenance identities (from 01d) -------------------------------------
op <- inv[is_operating == TRUE]
assert(nrow(op) == G(209, V2_N_OP) && uniqueN(op$zone_uid) == G(31, V2_HOST_OP), "operating scope must be %d / %d, got %d / %d", G(209, V2_N_OP), G(31, V2_HOST_OP), nrow(op), uniqueN(op$zone_uid))
assert(nrow(inv) == G(297, V2_N_ALL) && uniqueN(inv$zone_uid) == G(37, V2_HOST_ALL), "all scope must be %d / %d, got %d / %d", G(297, V2_N_ALL), G(37, V2_HOST_ALL), nrow(inv), uniqueN(inv$zone_uid))
sc_op <- op[, .N, by = src][, setNames(N, src)][c("census", "dcm", "crosscheck", "government")]; sc_op[is.na(sc_op)] <- 0L
assert(all(sc_op == G(c(61L, 148L, 0L, 0L), c(62L, 123L, 16L, 32L))), "operating source split (census/dcm/crosscheck/government) is %s", paste(sc_op, collapse = "/"))
sc_all <- inv[, .N, by = src][, setNames(N, src)][c("census", "dcm", "crosscheck", "government")]; sc_all[is.na(sc_all)] <- 0L
assert(all(sc_all == G(c(67L, 230L, 0L, 0L), c(66L, 202L, 29L, 32L))), "all-scope source split is %s", paste(sc_all, collapse = "/"))
assert(inv[status == "construction", .N] == G(50, 60) && inv[status == "announced", .N] == G(38, 36), "pipeline must be %d + %d", G(50, 60), G(38, 36))
assert(op[!is.na(mw_reported), .N] == G(43, 123) && abs(op[, sum(mw_reported, na.rm = TRUE)] - G(734.19, 1383.21)) < 0.01,
       "operating reported IT load must be %d rows / %.2f MW", G(43, 123), G(734.19, 1383.21))
tc <- op[, .N, by = dc_type][, setNames(N, dc_type)][c("colocation","enterprise","telecom","hyperscale","government")]; tc[is.na(tc)] <- 0L; names(tc)[5] <- "government"
msg("operating type counts: %s", paste(names(tc), tc, collapse = ", "))
assert(identical(tc, G(c(colocation=110L, enterprise=59L, telecom=32L, hyperscale=8L, government=0L), c(colocation=127L, enterprise=47L, telecom=20L, hyperscale=7L, government=32L))), "operating type counts differ from the expected split")

# ---- write both scopes (scope-independent script: writes outputs/ and outputs_all/) --
cols <- c("dc_id","facility_id","name","operator","status","is_operating","src","dc_type","opened_year",
          "mw_reported","mw_reported_buildout","no_derived_mw","lon","lat","zone_uid","dist_name","state_name","geocode_class")
for (sc in c("operating", "all")) {
  x <- if (sc == "operating") inv[is_operating == TRUE, ..cols] else inv[, ..cols]
  od <- dirname(outp("x", sc)); dir.create(od, showWarnings = FALSE, recursive = TRUE)
  fwrite(x, outp("dc_current_inventory.csv", sc))
  sfx <- st_as_sf(x, coords = c("lon", "lat"), crs = 4326, remove = FALSE)
  saveRDS(sfx, outp(siting_points_file, sc))
  if (file.exists(outp("dc_corroborated_points.gpkg", sc))) unlink(outp("dc_corroborated_points.gpkg", sc))
  st_write(sfx, outp("dc_corroborated_points.gpkg", sc), quiet = TRUE)
  msg("%s: %d facilities in %d districts -> %s", sc, nrow(x), uniqueN(x$zone_uid), basename(od))
}

# ---- summary tables ------------------------------------------------------------------
top <- op[, .N, by = dist_name][order(-N)]
msg("top six hosting districts: %s = %d/%d (%.0f%%)", paste(top$dist_name[1:6], collapse = ", "),
    sum(top$N[1:6]), nrow(op), 100 * sum(top$N[1:6]) / nrow(op))
fwrite(inv[, .N, by = .(status, src, dc_type)][order(status, src, dc_type)], out("tab01_inventory_summary.csv"))
