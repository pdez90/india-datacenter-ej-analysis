# =============================================================================
# 16d_postprocess_inmap.R  —  InMAP output -> district exposure, national summary, GEMM deaths
# -----------------------------------------------------------------------------
# Reads the InMAP field written by 16c (inmap_work/dc_inmap_output_<INVENTORY>.shp; TotalPM25 is the
# PM2.5 increment from the data centres' marginal power-plant emissions, TotalPop is GPW v4.11 2020
# population carried by the InMAP grid) and
#   1. refuses to run if the field was made for another scope or inventory, or is older than the
#      emissions file (inmap_work/ is shared between scopes);
#   2. intersects grid cells with the 641 districts (Census 2011 geography). Each piece carries
#      popw = TotalPop x (piece area / cell area);
#   3. district means: dpm25 = sum(popw x TotalPM25) / sum(popw); person_ugm3 = sum(popw x TotalPM25);
#   4. national population-weighted mean from the grid pieces ("grid") and from district means x
#      NFHS-frame population ("district-mean equivalent"); population above 0.01 and 0.10 ug/m3;
#   5. exposure-weighted receiving profile: district shares weighted by pop_2020 x dpm25;
#   6. GEMM attributable deaths (Burnett et al. 2018, NCD+LRI, adults 25+), per grid piece with the
#      district's ACAG 2013-2022 mean as the baseline z:
#        deaths = popw x ADULT_SHARE x BASE_RATE x [HR(z + dz) / HR(z) - 1]
#        HR(z)  = exp(theta log(1 + z'/alpha) / (1 + exp(-(z' - mu)/nu))), z' = max(z - 2.4, 0)
#      theta 0.143, alpha 1.6, mu 15.5, nu 36.8; ADULT_SHARE 0.562 (UN WPP 2022); BASE_RATE 0.0087
#      (GBD 2023; upper end of 0.0084-0.0087 for India 25+, 2019-2023). Same parameters as 13_burden (which uses a WHO 5 ug/m3 counterfactual instead).
# Districts whose cells carry no gridded population use the area-weighted mean.
# Guards: gridded population over the districts 0.8-2.2 bn (units); <= 5 M people outside any
#   district piece; 641 of 641 districts covered.
# Self-test (every run): the same formulas applied to the submitted paper's district increments
#   (data/golden/districts.geojson, run 3) must give 0.0583 ug/m3, 1,976 deaths (+-1%), receiving
#   SC/ST 0.32, BPL 0.47, wealth 261,099. (Run 3's grid file no longer exists, so the grid-level
#   step itself cannot be replayed; its district-mean result is what is checked.)
# Outputs (dir_out): tab16_inmap_summary.txt, tab16_inmap_district.csv, tab16_receiving_profile.csv
# Submitted paper (run 3, operating): grid 0.0585 / district 0.0583 ug/m3; 1,234 M >= 0.01;
#   179 M >= 0.10; 1,976 deaths; receiving SC/ST 0.32, BPL 0.47, wealth 261,099. All scope: 0.1352, 4,629.
# =============================================================================
source(file.path(Sys.getenv("DCEJ_ROOT", unset = (function(){a<-commandArgs(FALSE);f<-sub("^--file=","",grep("^--file=",a,value=TRUE));d<-if(length(f)) dirname(normalizePath(f[1])) else getwd();while(!file.exists(file.path(d,"scripts","00_config.R"))&&d!=dirname(d)) d<-dirname(d);d})()), "scripts", "00_config.R"))
suppressMessages(library(sf)); sf_use_s2(FALSE)

ADULT_SHARE <- 0.562; BASE_RATE <- 0.0087
gemm_hr <- function(z, theta = 0.143, alpha = 1.6, mu = 15.5, nu = 36.8, tmrel = 2.4) {
  z <- pmax(z - tmrel, 0); exp(theta * log(1 + z / alpha) / (1 + exp(-(z - mu) / nu)))
}
gemm_deaths <- function(pop, z, dz) ifelse(is.na(z) | is.na(dz), 0, pop * ADULT_SHARE * BASE_RATE * (gemm_hr(z + dz) / gemm_hr(z) - 1))
profile <- function(d, w) {   # exposure-weighted shares; w = pop_2020 x dpm25
  f <- function(x) { ok <- !is.na(x) & !is.na(w); sum(w[ok] * x[ok]) / sum(w[ok]) }
  data.table(scst = f(d$share_scst_2019), bpl = f(d$share_bpl_2019), wealth = f(d$wealth_mean_2019),
             muslim = f(d$share_muslim_2019), no_electricity = f(d$share_no_electricity_2019), urban = f(d$urban_share_2019))
}

# ---- 0. self-test of the formulas on the submitted paper's district increments --------------
gold_path <- file.path(dir_data, "golden", "districts.geojson")
if (file.exists(gold_path)) {
  g <- as.data.table(st_drop_geometry(st_read(gold_path, quiet = TRUE)))
  inc <- if (DC_SCOPE == "operating") "dpm25" else "dpm25_all"
  pw  <- g[, sum(pop_2020 * get(inc)) / sum(pop_2020)]
  dth <- g[, sum(gemm_deaths(pop_2020, pm25_acag_local, get(inc)))]
  pr  <- profile(g, g$pop_2020 * g[[inc]])
  msg("self-test on run 3 district increments: %.4f ug/m3, %.0f deaths, receiving SC/ST %.2f BPL %.2f wealth %.0f",
      pw, dth, pr$scst, pr$bpl, pr$wealth)
  want <- if (DC_SCOPE == "operating") c(1976, 0.0583) else c(4629, NA)
  assert(abs(dth / want[1] - 1) < 0.01, "GEMM self-test: %.0f deaths vs %d in the submitted paper", dth, want[1])
  if (DC_SCOPE == "operating") {
    assert(abs(pw - want[2]) < 5e-4, "district-mean self-test: %.4f vs 0.0583", pw)
    assert(abs(pr$wealth - 261099) < 500 && abs(pr$scst - 0.32) < 0.005 && abs(pr$bpl - 0.47) < 0.006,
           "receiving-profile self-test failed")
  }
}

# ---- 1. inputs and staleness/scope guards ----------------------------------------------------
# Sensitivity runs (10 Oct 2026, reviewer request): DCEJ_SENS=<name> keeps every output of this
# script apart from the baseline by suffixing file names with _<name> (see scripts/run_sensitivity.R).
SENS <- Sys.getenv("DCEJ_SENS", ""); SFX <- if (nzchar(SENS)) paste0("_", SENS) else ""
OUT_TAG <- paste0(INVENTORY, if (DC_SCOPE == "all") "_all" else "", SFX)
OUT  <- file.path(dir_inmap, sprintf("dc_inmap_output_%s.shp", OUT_TAG))
if (!file.exists(OUT) && is_v1 && DC_SCOPE == "operating") OUT <- file.path(dir_inmap, "dc_inmap_output.shp")
assert(file.exists(OUT), "no InMAP output %s: run 16b and 16c for DC_SCOPE=%s first", OUT, DC_SCOPE)
stamp_f <- file.path(dir_inmap, sprintf("dc_inmap_scope_%s.txt", OUT_TAG))
if (!file.exists(stamp_f)) stamp_f <- file.path(dir_inmap, "dc_inmap_scope.txt")
stamp <- readLines(stamp_f)
assert(stamp[1] == DC_SCOPE, "%s says the field is for DC_SCOPE=%s, not %s: rerun 16b and 16c for this scope", basename(stamp_f), stamp[1], DC_SCOPE)
inv_line <- grep("^INVENTORY=", stamp, value = TRUE)
assert(!length(inv_line) || inv_line == paste0("INVENTORY=", INVENTORY), "the InMAP field was made with %s, not INVENTORY=%s", inv_line, INVENTORY)
# The field must be converged and must have been made from THIS scope's current emissions table
# (content hash written by 16b and carried into the field stamp by 16c; 3 Oct 2026).
assert(!any(grepl("NOT CONVERGED", stamp)), "%s is a NOT CONVERGED test field: rerun 16c to convergence", basename(OUT))
md5_line <- grep("^EMIS_MD5=", stamp, value = TRUE)
md5_now  <- paste0("EMIS_MD5=", unname(tools::md5sum(out(sprintf("tab16b_emissions_points%s.csv", SFX)))))
if (length(md5_line)) {
  assert(md5_line == md5_now, "the InMAP field was made from different emissions than this scope's tab16b_emissions_points.csv: rerun 16b -> 16c")
} else msg("WARNING: %s predates emission hashing (3 Oct 2026); cannot confirm it matches the current emissions", basename(stamp_f))

dist <- read_districts()
fr   <- fread(path_frame)
o <- st_read(OUT, quiet = TRUE)[, c("TotalPM25", "TotalPop")]
if (is.na(st_crs(o))) stop("InMAP output has no CRS (.prj missing)")
dist <- st_transform(dist, st_crs(o))
o <- o[lengths(st_intersects(o, st_as_sfc(st_bbox(dist)))) > 0, ]
o$cell <- seq_len(nrow(o)); o$cell_area <- as.numeric(st_area(o))
msg("%d InMAP cells over the India bounding box", nrow(o))

# ---- 2. cells x districts ----------------------------------------------------------------------
pc <- suppressWarnings(st_intersection(o, st_make_valid(dist[, c("zone_uid")])))
pc <- as.data.table(st_drop_geometry(transform(pc, frac = as.numeric(st_area(pc)) / cell_area)))
pc[, popw := TotalPop * pmin(frac, 1)]
pop_grid <- pc[, sum(popw)]
inside   <- o$cell %in% pc$cell
cell_pop_in_box <- sum(o$TotalPop[inside])
msg("gridded population over the districts %.3f bn (cells touching India hold %.3f bn)", pop_grid / 1e9, cell_pop_in_box / 1e9)
assert(pop_grid > 0.8e9 && pop_grid < 2.2e9, "gridded population %.2f bn outside 0.8-2.2 bn: TotalPop units?", pop_grid / 1e9)

# ---- 3. districts ------------------------------------------------------------------------------
pc[, areaw := frac * cell_area]
dd <- pc[, .(pop_grid = sum(popw), person_ugm3 = sum(popw * TotalPM25), cells = uniqueN(cell),
             area_mean = sum(areaw * TotalPM25) / sum(areaw)), by = zone_uid]
# A district whose cells carry no gridded population falls back to the area-weighted mean. In v1 this
# hit Chennai and Karaikal, whose warped NFHS polygons lay in the sea; with the Census 2011 boundaries
# (v2, 00a_districts.R) none is expected, and any that remains is named in the console.
dd[, dpm25 := fifelse(pop_grid > 0, person_ugm3 / pop_grid, area_mean)]
dd[, weighting := fifelse(pop_grid > 0, "population", "area (no gridded population)")]
dd <- merge(as.data.table(st_drop_geometry(dist))[, .(zone_uid, dist_name, state_name)], dd, by = "zone_uid", all.x = TRUE)
miss <- dd[is.na(dpm25)]
if (dd[weighting != "population", .N]) msg("area-weighted districts (no gridded population): %s", dd[weighting != "population", paste(dist_name, collapse = ", ")])
fr2 <- fr[, .(zone_uid, pop_2020, pm25_acag_local, share_scst_2019, share_bpl_2019, share_muslim_2019,
              share_no_electricity_2019, urban_share_2019, wealth_mean_2019, hosting, hosting_all)]
uncovered <- fr2[zone_uid %in% miss$zone_uid, sum(pop_2020, na.rm = TRUE)]
msg("districts covered by grid: %d of %d (uncovered population %.2f M)", nrow(dd) - nrow(miss), nrow(dd), uncovered / 1e6)
assert(uncovered <= 5e6, "%.1f M people in districts with no InMAP cell", uncovered / 1e6)
dd <- merge(dd, fr2, by = "zone_uid", all.x = TRUE)
dd[, state_name := state_fix(state_name, dist_name)]

# ---- 4. national -------------------------------------------------------------------------------
pc <- merge(pc, fr2[, .(zone_uid, pm25_acag_local)], by = "zone_uid", all.x = TRUE)
pwm_grid <- pc[, sum(popw * TotalPM25) / sum(popw)]
pwm_dist <- dd[!is.na(dpm25) & !is.na(pop_2020), sum(pop_2020 * dpm25) / sum(pop_2020)]
ge01 <- pc[TotalPM25 >= 0.01, sum(popw)]; ge10 <- pc[TotalPM25 >= 0.10, sum(popw)]
deaths <- pc[, sum(gemm_deaths(popw, pm25_acag_local, TotalPM25))]
dd <- merge(dd, pc[, .(deaths = sum(gemm_deaths(popw, pm25_acag_local, TotalPM25))), by = zone_uid], by = "zone_uid", all.x = TRUE)
pr <- profile(dd, dd$pop_2020 * dd$dpm25)
host <- if (DC_SCOPE == "operating") "hosting" else "hosting_all"
h <- dd[get(host) == 1]
host_wealth <- h[, weighted.mean(wealth_mean_2019, pop_2020, na.rm = TRUE)]

lines <- c(
  sprintf("InMAP post-processing (16d), DC_SCOPE=%s, INVENTORY=%s, %s", DC_SCOPE, INVENTORY, format(Sys.time(), "%Y-%m-%d %H:%M")),
  sprintf("InMAP field: %s (%s)", basename(OUT), paste(stamp[-1], collapse = "; ")),
  sprintf("Population-weighted mean PM2.5 increment (grid): %.4f ug/m3", pwm_grid),
  sprintf("  (district-mean equivalent, for comparison:     %.4f ug/m3)", pwm_dist),
  sprintf("Population exposed >= 0.01 ug/m3 (grid cells): %.0f million", ge01 / 1e6),
  sprintf("Population exposed >= 0.10 ug/m3 (grid cells): %.0f million", ge10 / 1e6),
  sprintf("Maximum district mean increment: %.3f ug/m3 (%s, %s)", max(dd$dpm25, na.rm = TRUE), dd[which.max(dpm25), dist_name], dd[which.max(dpm25), state_name]),
  sprintf("Exposure-weighted receiving profile: SC/ST %.2f | BPL %.2f | wealth %.0f", pr$scst, pr$bpl, pr$wealth),
  sprintf("  (hosting districts, population-weighted: %d districts, wealth %.0f)", nrow(h), host_wealth),
  sprintf("GEMM attributable deaths from the increment: %.0f /yr", deaths),
  sprintf("  (GEMM NCD+LRI, adults 25+: adult share %.3f, base rate %.4f, baseline z = district ACAG 2013-2022 mean)", ADULT_SHARE, BASE_RATE),
  sprintf("Districts covered by grid: %d of %d", nrow(dd) - nrow(miss), nrow(dd)),
  sprintf("Gridded population over the districts: %.3f bn", pop_grid / 1e9))
writeLines(lines, out(sprintf("tab16_inmap_summary%s.txt", SFX))); cat(paste(lines, collapse = "\n"), "\n")

fwrite(dd[, .(zone_uid, dist_name, state_name, pop_2020, pop_grid, cells, dpm25, person_ugm3,
              person_share = person_ugm3 / sum(person_ugm3, na.rm = TRUE), pm25_acag_local, deaths, hosting = get(host), weighting)],
       out(sprintf("tab16_inmap_district%s.csv", SFX)))
fwrite(cbind(scope = DC_SCOPE, inventory = INVENTORY, pr, hosting_wealth = host_wealth, pwm_grid, pwm_dist,
             pop_ge_0_01_million = ge01 / 1e6, pop_ge_0_10_million = ge10 / 1e6, deaths), out(sprintf("tab16_receiving_profile%s.csv", SFX)))

# Reference values of the submitted paper (run 3); the v2 field is new, so these are reported, not asserted.
if (DC_SCOPE == "operating")
  msg("submitted paper (run 3): 0.0585 / 0.0583 ug/m3, 1,234 M >= 0.01, 179 M >= 0.10, 1,976 deaths; this run: %.4f / %.4f, %.0f M, %.0f M, %.0f deaths (x%.2f)",
      pwm_grid, pwm_dist, ge01 / 1e6, ge10 / 1e6, deaths, deaths / 1976)
msg("16d done")
