# =============================================================================
# 06_analysis_frame.R  —  the district analysis frame (N_DIST rows: v2 641, v1 642)
# -----------------------------------------------------------------------------
# Joins, on zone_uid:
#   - facility counts for both scopes (01)                         dc_count, dc_count_all, hosting, hosting_all
#   - NFHS-5 social composition (05)                               *_2019
#   - locally rebuilt environment (04)                             pm25_acag_local, pm25_acag_unc, no2_surf, coal/gas/fossil MW, dist_fossil_km
#   - Earth Engine district exports (03, data/gee/*.csv) when present
#   - external layers (04b): pm25_lghap, o3_annual, o3_summer, rwi_mean
#   - rebuilt replacements for the formerly frozen columns (3 Oct 2026):
#       pop_2020       <- GHSL 2020 population (03, gee_ghsl_2020.csv; matches the frozen layer, median ratio 1.000)
#       hot_days_peryr <- ERA5-Land days >= 35 C (03, gee_era5_heat.csv; r 0.998 with the frozen layer)
#       bws_raw        <- WRI Aqueduct 4.0 district baseline water stress (05e, aqueduct_district.csv; r 1.000)
#       o3_summer, rwi_mean <- 04b
#   - FROZEN values from the 1 Sep 2026 pipeline (data/golden/districts.geojson) ONLY where a
#     rebuilt layer is missing on this machine; each such column is named in the console and in
#     analysis_frame_provenance.csv, so nothing is silently mixed.
# Writes data/derived/analysis_district.csv (+ .rds with geometry) and the
# provenance table.
# =============================================================================
source(file.path(Sys.getenv("DCEJ_ROOT", unset = (function(){a<-commandArgs(FALSE);f<-sub("^--file=","",grep("^--file=",a,value=TRUE));d<-if(length(f)) dirname(normalizePath(f[1])) else getwd();while(!file.exists(file.path(d,"scripts","00_config.R"))&&d!=dirname(d)) d<-dirname(d);d})()), "scripts", "00_config.R"))

dist <- read_districts()
fr <- as.data.table(st_drop_geometry(dist))[, .(zone_uid, dist_code, dist_name, state_name_2015 = state_name,
                                                state_name = state_fix(state_name, dist_name))]
prov <- list()
# Every district table merged here must cover exactly this district layer: the derived files are not
# inventory-suffixed, so one written by a v1 run (642 warped polygons) must never feed a v2 frame.
add <- function(dt, src, must_cover = TRUE) { cols <- setdiff(names(dt), "zone_uid")
  if (must_cover) assert(all(dt$zone_uid %in% fr$zone_uid) && !anyDuplicated(dt$zone_uid) && nrow(dt) >= 0.95 * nrow(fr),
                         "%s: %d districts, %d not in the current layer (e.g. %s): built on a different district layer? rerun its step",
                         src, nrow(dt), length(setdiff(dt$zone_uid, fr$zone_uid)), paste(head(setdiff(dt$zone_uid, fr$zone_uid), 3), collapse = ", "))
  fr <<- merge(fr, dt, by = "zone_uid", all.x = TRUE)
  prov[[length(prov) + 1]] <<- data.table(variable = cols, source = src) }

# ---- facility counts ---------------------------------------------------------------
for (sc in c("operating", "all")) {
  inv <- fread(outp("dc_current_inventory.csv", sc))
  cnt <- inv[, .N, by = zone_uid]; setnames(cnt, "N", if (sc == "operating") "dc_count" else "dc_count_all")
  add(cnt, sprintf("01_inventory.R (%s scope)", sc), must_cover = FALSE)
}
fr[is.na(dc_count), dc_count := 0L]; fr[is.na(dc_count_all), dc_count_all := 0L]
fr[, `:=`(hosting = as.integer(dc_count > 0), hosting_all = as.integer(dc_count_all > 0))]

# ---- NFHS-5 and local environment -----------------------------------------------------
add(fread(file.path(dir_derived, "nfhs5_district.csv")), "05_nfhs_district.R (NFHS-5 2019-21)")
add(fread(file.path(dir_derived, "env_local_district.csv")), "04_env_local.R (ACAG V5GL06, Anenberg NO2, WRI GPPD)")
ext_f <- file.path(dir_derived, "env_external_district.csv")
if (file.exists(ext_f)) add(fread(ext_f)[, !"rwi_tiles"], "04b_env_external.R (LGHAP v2, Wang et al. 2025 ozone, Meta RWI)") else
  msg("WARNING: %s missing (run 04b): LGHAP, annual ozone and the rebuilt summer ozone / RWI are unavailable", basename(ext_f))
aq_f <- file.path(dir_derived, "aqueduct_district.csv")
if (file.exists(aq_f)) add(fread(aq_f)[, .(zone_uid, bws_raw = bws_raw_aq)], "05e_water.R (WRI Aqueduct 4.0 district baseline water stress)")

# ---- Earth Engine exports (03_gee_extract.R) -----------------------------------
gee_files <- list.files(dir_gee, pattern = "^gee_.*\\.csv$", full.names = TRUE)
for (f in gee_files) {
  g <- fread(f); keep <- setdiff(names(g), c("system:index", ".geo", "dist_name", "state_name", "dist_code"))
  g <- g[, ..keep]; dup <- intersect(setdiff(names(g), "zone_uid"), names(fr))
  if (length(dup)) g[, (dup) := NULL]
  add(g, sprintf("03 Earth Engine export: %s", basename(f)))
}

# ---- rebuilt replacements for formerly frozen columns ----------------------------------------
if ("ghsl_pop_2020" %in% names(fr)) { fr[, pop_2020 := ghsl_pop_2020]; prov[[length(prov) + 1]] <- data.table(variable = "pop_2020", source = "03 GHSL 2020 population (= ghsl_pop_2020)") }
if ("hot_days_peryr_era5" %in% names(fr)) { fr[, hot_days_peryr := hot_days_peryr_era5]; prov[[length(prov) + 1]] <- data.table(variable = "hot_days_peryr", source = "03 ERA5-Land hot days (= hot_days_peryr_era5)") }

# ---- frozen fallbacks (only for layers not rebuilt on this machine) --------------------------
frozen_vars <- c("pop_2020", "rwi_mean", "o3_summer", "hot_days_peryr", "bws_raw")
need <- setdiff(frozen_vars, names(fr))
if (length(need) && !is_v1 && Sys.getenv("DCEJ_ALLOW_FROZEN") != "1")
  stop(sprintf("v2 cannot use the frozen layer for %s: it was computed on the warped NFHS polygons. Rebuild it (03 / 04b / 05e) or set DCEJ_ALLOW_FROZEN=1 for a test run.",
               paste(need, collapse = ", ")), call. = FALSE)
if (length(need)) {
  gold <- as.data.table(st_drop_geometry(st_read(file.path(dir_data, "golden", "districts.geojson"), quiet = TRUE)))
  add(gold[, c("zone_uid", need), with = FALSE], must_cover = FALSE, "FROZEN: 1 Sep 2026 pipeline output (app layer) - regenerate via 03/05e")
  msg("using FROZEN values for: %s", paste(need, collapse = ", "))
}

# ---- derived variables --------------------------------------------------------------------
fr[, pop_million := pop_2020 / 1e6]
fr[, wealth_pct := 100 * frank(wealth_mean_2019, na.last = "keep", ties.method = "average") / sum(!is.na(wealth_mean_2019))]
fr[, coal_district := as.integer(coal_mw >= 1000)]
fr[, class4 := fcase(hosting == 1 & coal_district == 0, "DC only", hosting == 0 & coal_district == 1, "coal only",
                     hosting == 1 & coal_district == 1, "both", default = "neither")]

setorder(fr, zone_uid)
fwrite(fr, path_frame)
saveRDS(merge(dist[, "zone_uid"], fr, by = "zone_uid"), sub("csv$", "rds", path_frame))
pv <- rbindlist(prov); fwrite(pv, file.path(dir_derived, "analysis_frame_provenance.csv"))
msg("analysis frame: %d districts x %d columns; hosting %d (operating) / %d (all)", nrow(fr), ncol(fr), sum(fr$hosting), sum(fr$hosting_all))
assert(nrow(fr) == N_DIST && sum(fr$hosting) == G(31, V2_HOST_OP) && sum(fr$hosting_all) == G(37, V2_HOST_ALL),
       "frame must have %d districts, %d/%d hosting", N_DIST, G(31, V2_HOST_OP), G(37, V2_HOST_ALL))
print(fr[, .N, by = class4])
