# =============================================================================
# 16b_build_emissions.R  —  attributable marginal emissions -> power-plant point sources
# -----------------------------------------------------------------------------
# SI section S4.1. Takes the load-state attributable marginal emissions from 15b
# (SO2, NOx, primary PM2.5; tonnes/yr) and places them on India's coal and gas
# plants (WRI GPPD, capacity > 0: 253 coal / 204,919 MW, 68 gas / 24,948 MW) as
# elevated point sources for Global InMAP (16c).
#
# Allocation rule — recovered EXACTLY from the app's plant layer (plants.geojson,
# max relative error 3e-12 for all 321 plants x 3 pollutants, 30 Sep 2026):
#   1. Each load state's tonnes are split between the gas and coal fleets with a
#      FIXED NATIONAL fraction per pollutant (not the state's capacity mix):
#        gas share f = r/(1+r), gas:coal ratio r = 1/5750 SO2, 1/57.5 NOx, 1/230 PM2.5
#        -> gas carries 0.0174% of SO2, 1.709% of NOx, 0.433% of PM2.5
#        (the notes: "gas contributes 0.02% of SO2, 1.7% of NOx, 0.4% of PM2.5").
#   2. Each fleet share goes to that state's own plants of that fuel, in proportion
#      to nameplate capacity.
#   3. A state with no plant of that fuel (operating load states; no gas: Bihar, Chhattisgarh,
#      Himachal Pradesh, Odisha, Punjab, Telangana, West Bengal; no coal: Goa, Kerala, Puducherry, Tripura;
#      neither: Arunachal Pradesh, Chandigarh, Nagaland, Uttarakhand) passes the share to a national
#      pool, spread over ALL plants of that fuel in proportion to capacity.
# Plant state = the NFHS district shapefile's state label (2015 geography). v1: Telangana
# plants sit in Andhra Pradesh, as do the v1 load states. v2: Telangana districts are
# labelled Telangana for both the load (15b) and the plants, so Telangana's emissions go
# to Telangana's own plants (decided 3 Oct 2026). 18 coastal plants fall
# just outside the polygons and take the nearest district.
# Stacks: 200 m coal, 100 m gas (Peshin et al. 2024); no diameter/temperature/
# velocity, so InMAP's ASME plume rise adds ~nothing and the plume sits at the stack.
#
# InMAP UNITS: the shapefile is written in kg/year and 16c sets EmissionUnits =
# "kg/year". InMAP's "tons/year" means SHORT tons (907.18 kg); writing metric tonnes
# under "tons/year" would understate every species by 9.3%.
#
# Outputs: inmap_work/dc_emissions_<INVENTORY>[_all].shp (one per scope, so the two scopes never
# overwrite each other) + its _scope.txt stamp, and in dir_out
# tab16b_emissions_points.csv, tab16b_emissions_by_state.csv, tab16b_source_split.csv.
# INMAP_EXPORT_ONLY=1 writes only the tables. inmap_work/ is shared between scopes:
# run the operating 16b -> 16c before the operating 16d.
# Golden (v1 inventory, operating): 109,802 t SO2, 58,659 t NOx, 8,102 t PM2.5; every plant's
# tonnes equal plants.geojson.
# =============================================================================
source(file.path(Sys.getenv("DCEJ_ROOT", unset = (function(){a<-commandArgs(FALSE);f<-sub("^--file=","",grep("^--file=",a,value=TRUE));d<-if(length(f)) dirname(normalizePath(f[1])) else getwd();while(!file.exists(file.path(d,"scripts","00_config.R"))&&d!=dirname(d)) d<-dirname(d);d})()), "scripts", "00_config.R"))

POLS      <- c("so2", "nox", "pm25")
GAS_RATIO <- c(so2 = 1 / 5750, nox = 1 / 57.5, pm25 = 1 / 230)   # gas : coal, national
GAS_FRAC  <- GAS_RATIO / (1 + GAS_RATIO)
STACK_M   <- c(Coal = 200, Gas = 100)
dir.create(dir_inmap, showWarnings = FALSE)
# Each scope writes its own shapefile (EMIS_TAG below carries _all for the all scope), so
# run_all.R cannot leave the all-scope field where the operating 16c expects its own.
export_only <- Sys.getenv("INMAP_EXPORT_ONLY") == "1"
# Sensitivity runs (10 Oct 2026, reviewer request): DCEJ_SENS=<name> keeps every output of this
# script apart from the baseline by suffixing file names with _<name> (see scripts/run_sensitivity.R).
SENS <- Sys.getenv("DCEJ_SENS", ""); SFX <- if (nzchar(SENS)) paste0("_", SENS) else ""
EMIS_TAG <- paste0(INVENTORY, if (DC_SCOPE == "all") "_all" else "", SFX)
ALLOC_W <- if (SENS == "alloc_gen") "estimated_generation_gwh_2017" else "capacity_mw"   # plant allocation weight within a state
# (alloc_gen is an optional extra variant; the paper does not report it and run_sensitivity.R does not run it)
if (nzchar(SENS)) msg("SENSITIVITY RUN %s: allocation weight = %s", SENS, ALLOC_W)

# ---- plants: GPPD India coal + gas, capacity > 0, state from the district polygons ----------
g <- fread(file.path(dir_data, "raw", "gppd.csv"))[country == "IND" & primary_fuel %in% c("Coal", "Gas") & capacity_mw > 0]
assert(g[primary_fuel == "Coal", .N] == 253 && g[primary_fuel == "Gas", .N] == 68, "expected 253 coal and 68 gas plants")
if (SENS == "alloc_gen") assert(all(is.finite(g$estimated_generation_gwh_2017) & g$estimated_generation_gwh_2017 > 0), "alloc_gen: every plant needs a positive estimated_generation_gwh_2017")

# ---- documented coordinate corrections to GPPD (v2 only) ------------------------------------
# GPPD's WRI coordinates put a small number of plants in the wrong state. Each correction names
# the plant, the field, the old and the new value, the reason and the evidence URL, exactly like
# data/public/inventory_corrections_v2.csv, and the old value must still be there or the run stops.
# v1 is left untouched so the submitted paper, and the plants.geojson golden check below, still
# reproduce. Found 21 Oct 2026 by cross-checking every plant name against the Census district names.
if (!is_v1) {
  pc <- fread(file.path(dir_data, "public", "plant_corrections.csv"), colClasses = "character")
  g <- copy(g)                      # g came from a filtered fread; copy() guarantees set() updates it in place
  for (i in seq_len(nrow(pc))) {
    e <- pc[i]; r <- which(g$gppd_idnr == e$gppd_idnr)
    assert(length(r) == 1, "plant correction: gppd_idnr %s matches %d rows", e$gppd_idnr, length(r))
    assert(e$field %in% names(g), "plant correction: unknown field %s", e$field)
    assert(abs(as.numeric(g[[e$field]][r]) - as.numeric(e$old_value)) < 1e-9,
           "plant correction %s %s: expected old value %s, found %s", e$gppd_idnr, e$field, e$old_value, as.character(g[[e$field]][r]))
    set(g, r, e$field, as.numeric(e$new_value))
  }
  msg("GPPD coordinate corrections applied: %d field(s) on %d plant(s) (%s)",
      nrow(pc), uniqueN(pc$gppd_idnr), paste(unique(pc$name), collapse = ", "))
}
pts <- st_as_sf(g[, .(name, gppd_idnr, fuel = primary_fuel, capacity_mw, alloc_w = get(ALLOC_W), lon = longitude, lat = latitude)],
                coords = c("lon", "lat"), crs = 4326, remove = FALSE)
d <- read_districts()
pts_d <- st_transform(pts, st_crs(d))
k <- st_intersects(pts_d, d)
k <- vapply(seq_along(k), function(i) if (length(k[[i]])) k[[i]][1] else NA_integer_, 1L)
miss <- is.na(k); if (any(miss)) k[miss] <- st_nearest_feature(pts_d[miss, ], d)
msg("plants: %d in a district polygon, %d by nearest district (coastal)", sum(!miss), sum(miss))
p <- as.data.table(st_drop_geometry(pts))
p[, `:=`(zone_uid = d$zone_uid[k], state = d$state_name[k], dist_name = d$dist_name[k])]     # raw shapefile label = 2015 geography
if (!is_v1) p[, state := state_fix(state, dist_name)]          # v2: Telangana plants carry Telangana (matches 15b load states)

# ---- load-state attributable tonnes (15b) ---------------------------------------------------
m <- fread(out("tab15b_marginal.csv"))
E <- m[, .(so2 = sum(so2_t), nox = sum(nox_t), pm25 = sum(pm25_t)), by = .(load_state = load_state_2015)]
if (startsWith(SENS, "w_")) {   # alternative operator-class capacity weights: load-state tonnes from 15c
  wt <- fread(out("tab15c_weight_state_tonnes.csv"))[scheme == sub("^w_", "", SENS)]
  assert(nrow(wt) > 0, "no scheme %s in tab15c_weight_state_tonnes.csv (run 15c)", sub("^w_", "", SENS))
  E <- wt[, .(load_state = load_state_2015, so2 = so2_t, nox = nox_t, pm25 = pm25_t)]
  msg("SENSITIVITY RUN %s: load-state tonnes from 15c scheme %s (SO2 %.0f t vs baseline %.0f t)", SENS, sub("^w_", "", SENS), sum(E$so2), m[, sum(so2_t)])
}
if (SENS == "unresolved_zero") {   # 25 Oct 2026: facilities the row-by-row review could not confirm or refute carry no load.
  # Their tonnes are removed from the load-state totals; every other facility keeps its baseline MW (no
  # recalibration to the 1,800 MW anchor), so this measures their contribution alone (data/public/unresolved_facilities_20261025.csv).
  ur_f <- file.path(dir_data, "public", "unresolved_facilities_20261025.csv")   # authors' build file
  if (!file.exists(ur_f)) ur_f <- file.path(dir_data, "unresolved_facilities.csv")   # the released list (companion app repo, data/)
  assert(file.exists(ur_f), "unresolved_zero needs data/unresolved_facilities.csv from https://github.com/pdez90/india-datacentre-ej")
  ur <- fread(ur_f, colClasses = "character")
  ci <- fread(out("dc_current_inventory.csv"))[, .(dc_id, facility_id)]
  mm <- merge(m, ci, by = "dc_id", all.x = TRUE); assert(!anyNA(mm$facility_id), "16b unresolved_zero: dc_id without facility_id")
  drop <- mm[facility_id %in% ur$facility_id]
  assert(nrow(drop) == ur[facility_id %in% ci$facility_id, .N], "16b unresolved_zero: unresolved list does not match the operating inventory")
  E <- mm[!facility_id %in% ur$facility_id, .(so2 = sum(so2_t), nox = sum(nox_t), pm25 = sum(pm25_t)), by = .(load_state = load_state_2015)]
  msg("SENSITIVITY RUN %s: %d unresolved operating facilities set to zero (%.1f MW, %.0f t SO2 = %.1f%% of baseline); other facilities unchanged",
      SENS, nrow(drop), drop[, sum(mw_it)], drop[, sum(so2_t)], 100 * drop[, sum(so2_t)] / m[, sum(so2_t)])
}
unk <- setdiff(E$load_state, unique(p$state))
# v2: Chandigarh and Puducherry (government data centres) have no coal or gas plant of their own;
# rule 3 applies to both fuels, so their shares go to the national pools.
if (length(unk)) msg("load states with no plant of either fuel -> national pools for both fuels (rule 3): %s", paste(unk, collapse = ", "))
if (is_v1) assert(!length(unk), "unexpected load states with no plant: %s", paste(unk, collapse = ", "))   # v2: listed above; rule 3 handles them

# ---- allocation ---------------------------------------------------------------------------
split_rows <- list()
for (pl in POLS) {
  tt <- numeric(nrow(p))
  for (fu in c("Coal", "Gas")) {
    sh  <- if (fu == "Gas") GAS_FRAC[[pl]] else 1 - GAS_FRAC[[pl]]
    isf <- p$fuel == fu; has <- unique(p$state[isf]); pool <- 0
    for (i in seq_len(nrow(E))) {
      s <- E$load_state[i]; v <- sh * E[[pl]][i]
      own <- s %in% has
      if (own) { kk <- isf & p$state == s; tt[kk] <- tt[kk] + v * p$alloc_w[kk] / sum(p$alloc_w[kk]) } else pool <- pool + v
      split_rows[[length(split_rows) + 1]] <- data.table(pollutant = pl, load_state = s, fuel = fu, tonnes = v,
                                                         route = if (own) "own-state plants" else "national fleet pool")
    }
    tt[isf] <- tt[isf] + pool * p$alloc_w[isf] / sum(p$alloc_w[isf])
  }
  set(p, j = paste0(pl, "_t"), value = tt)
  assert(abs(sum(tt) - sum(E[[pl]])) < 1e-6 * sum(E[[pl]]), "%s: plant total does not conserve the load-state total", pl)
}
p[, height_m := STACK_M[fuel]]

# ---- check against the app's plant layer (v1 inventory only) -----------------------------------
if (DC_SCOPE == "operating" && is_v1 && !nzchar(SENS)) {
  gold <- as.data.table(st_drop_geometry(st_read(file.path(dir_data, "golden", "plants.geojson"), quiet = TRUE)))
  gold[, name := gsub('"+', '"', name)]; pn <- copy(p)[, name := gsub('"+', '"', name)]
  cmp <- merge(pn, gold, by = c("name", "fuel", "capacity_mw"), suffixes = c("", "_gold"))
  assert(nrow(cmp) == 321, "only %d of 321 plants matched the golden layer", nrow(cmp))
  assert(all(cmp$state == cmp$state_gold), "plant state labels differ from the golden layer")
  rel <- cmp[, max(abs(c(so2_t / so2_t_gold, nox_t / nox_t_gold, pm25_t / pm25_t_gold) - 1))]
  assert(rel < 1e-6, "plant tonnes differ from plants.geojson (max rel. error %.2g)", rel)
  msg("plant tonnes match plants.geojson for all 321 plants (max rel. error %.1g)", rel)
}
tot <- p[, lapply(.SD, sum), .SDcols = paste0(POLS, "_t")]
msg("emitted: %.0f t SO2, %.0f t NOx, %.0f t PM2.5 (gas: %.2f%% / %.2f%% / %.2f%%)",
    tot$so2_t, tot$nox_t, tot$pm25_t, 100 * GAS_FRAC[["so2"]], 100 * GAS_FRAC[["nox"]], 100 * GAS_FRAC[["pm25"]])

# ---- tables ---------------------------------------------------------------------------------
fwrite(p[order(state, fuel, -capacity_mw), .(name, gppd_idnr, fuel, state, zone_uid, capacity_mw, lon, lat, height_m, so2_t, nox_t, pm25_t)],
       out(sprintf("tab16b_emissions_points%s.csv", SFX)))
bys <- p[, .(n_plants = .N, coal_mw = sum(capacity_mw[fuel == "Coal"]), gas_mw = sum(capacity_mw[fuel == "Gas"]),
             so2_t = sum(so2_t), nox_t = sum(nox_t), pm25_t = sum(pm25_t)), by = .(plant_state = state)]
bys <- merge(bys, E[, .(plant_state = load_state, load_so2_t = so2, load_nox_t = nox, load_pm25_t = pm25)], by = "plant_state", all = TRUE)
for (v in c("so2_t", "nox_t", "pm25_t", "load_so2_t", "load_nox_t", "load_pm25_t")) set(bys, which(is.na(bys[[v]])), v, 0)
bys[, `:=`(so2_share = so2_t / sum(so2_t), nox_share = nox_t / sum(nox_t), pm25_share = pm25_t / sum(pm25_t))]
fwrite(bys[order(-so2_t)], out(sprintf("tab16b_emissions_by_state%s.csv", SFX)))
fwrite(rbindlist(split_rows), out(sprintf("tab16b_source_split%s.csv", SFX)))
print(bys[order(-so2_t), .(plant_state, so2_t = round(so2_t), load_so2_t = round(load_so2_t), so2_share = round(100 * so2_share, 1))])

# ---- InMAP emissions shapefile (kg/year) ----------------------------------------------------
if (!export_only) {
  shp <- st_as_sf(p[, .(name = substr(name, 1, 60), fuel, state, cap_mw = capacity_mw,
                        VOC = 0, NOx = nox_t * 1000, NH3 = 0, SOx = so2_t * 1000, PM2_5 = pm25_t * 1000,
                        Height = height_m, lon, lat)], coords = c("lon", "lat"), crs = 4326)
  f <- file.path(dir_inmap, sprintf("dc_emissions_%s.shp", EMIS_TAG))
  st_write(shp, f, delete_dsn = file.exists(f), quiet = TRUE)
  writeLines(c(DC_SCOPE, format(Sys.time()), "EmissionUnits = kg/year", paste0("INVENTORY=", INVENTORY),
               paste0("EMIS_MD5=", unname(tools::md5sum(out(sprintf("tab16b_emissions_points%s.csv", SFX))))), if (nzchar(SENS)) paste0("SENS=", SENS)),
             file.path(dir_inmap, sprintf("dc_emissions_%s_scope.txt", EMIS_TAG)))
  msg("wrote %s (%d point sources, scope %s, kg/year)", f, nrow(shp), DC_SCOPE)
}
