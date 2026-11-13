# =============================================================================
# 21_benefit_burden.R  —  who uses AI vs who breathes the data centres' power-plant pollution
# -----------------------------------------------------------------------------
# A use-vs-burden ledger, not an attribution of emissions to users. For the four states with the
# largest Claude.ai use shares (Anthropic 2026: MH 15.5%, TN 13.2%, KA 12.7%, DL 10.5%; 52% together)
# and the rest of India, it sets side by side:
#   use share | population share (NFHS frame, 2020) | operating facility share (01) |
#   SO2 / NOx / PM2.5 emitted by power plants in the state (16b, plant state = "where emitted") |
#   exposure received = state share of person x ug/m3 of the InMAP increment (16d, gridded population).
# Benefit-burden ratio = use share / exposure share; usage-per-capita index = use share / population share.
# Telangana districts are reported as Telangana (district crosswalk), Delhi as "Nct Of Delhi".
# [GAP] The full state use-share table of the source release is not recorded; only the four states.
# Self-test: with the run-3 increments, v1 inventory and v1 emissions the four states give use 52 /
#   population 21 / facilities 62 / SO2 69 / exposure 47%; ratios DL 6.04, KA 1.59, MH 1.34, TN 0.51,
#   rest 0.91 (to 0.02); TN exposure 25.9%, DL 1.7%.
# Output: tab21_benefit_burden.csv
# =============================================================================
source(file.path(Sys.getenv("DCEJ_ROOT", unset = (function(){a<-commandArgs(FALSE);f<-sub("^--file=","",grep("^--file=",a,value=TRUE));d<-if(length(f)) dirname(normalizePath(f[1])) else getwd();while(!file.exists(file.path(d,"scripts","00_config.R"))&&d!=dirname(d)) d<-dirname(d);d})()), "scripts", "00_config.R"))
suppressMessages(library(sf))

USE <- c(Maharashtra = 0.155, `Tamil Nadu` = 0.132, Karnataka = 0.127, `Nct Of Delhi` = 0.105)
ledger <- function(dist, fac, emis) {   # dist: zone_uid, state_name, pop_2020, person_ugm3
  grp <- function(s) fifelse(s %in% names(USE), s, "Rest of India")
  dist[, g := grp(state_name)]; fac[, g := grp(state_name)]; emis[, g := grp(plant_state)]
  a <- dist[, .(population = sum(pop_2020, na.rm = TRUE), person_ugm3 = sum(person_ugm3, na.rm = TRUE)), by = g]
  a <- merge(a, fac[, .(facilities = .N), by = g], by = "g", all.x = TRUE)
  a <- merge(a, emis[, .(so2_t = sum(so2_t), nox_t = sum(nox_t), pm25_t = sum(pm25_t)), by = g], by = "g", all.x = TRUE)
  for (v in c("facilities", "so2_t", "nox_t", "pm25_t")) set(a, which(is.na(a[[v]])), v, 0)
  a[, use_share := fifelse(g %in% names(USE), USE[g], 1 - sum(USE))]
  for (v in c("population", "person_ugm3", "facilities", "so2_t", "nox_t", "pm25_t")) a[, (paste0(v, "_share")) := get(v) / sum(get(v))]
  a[, `:=`(benefit_burden_ratio = use_share / person_ugm3_share, use_per_capita_index = use_share / population_share)]
  setnames(a, c("g", "person_ugm3_share"), c("state", "exposure_share"))
  a <- a[match(c(names(USE), "Rest of India"), state)]
  four <- a[state %in% names(USE), lapply(.SD, sum), .SDcols = c("use_share", "population_share", "facilities_share", "so2_t_share", "nox_t_share", "pm25_t_share", "exposure_share")]
  rbind(a, cbind(state = "Four use states", four, benefit_burden_ratio = four$use_share / four$exposure_share,
                 use_per_capita_index = four$use_share / four$population_share), fill = TRUE)
}
dist_of <- function(frame, inc) {
  d <- merge(fread(frame)[, .(zone_uid, dist_name, state_name, pop_2020)], inc, by = "zone_uid")
  d[, state_name := state_fix(state_name, dist_name)]; d[, person_ugm3 := pop_2020 * dpm25]; d
}

# ---- self-test on run 3 ----------------------------------------------------------------------
gold_path <- file.path(dir_data, "golden", "districts.geojson")
v1_emis <- file.path(ROOT, "outputs_v1", "tab16b_emissions_by_state.csv")
v1_fac  <- file.path(ROOT, "outputs_v1", "dc_current_inventory.csv")
if (DC_SCOPE == "operating" && all(file.exists(c(gold_path, v1_emis, v1_fac, file.path(dir_derived, "analysis_district_v1.csv"))))) {
  g <- as.data.table(st_drop_geometry(st_read(gold_path, quiet = TRUE)))[, .(zone_uid, dpm25)]
  t1 <- ledger(dist_of(file.path(dir_derived, "analysis_district_v1.csv"), g), fread(v1_fac), fread(v1_emis))
  r <- setNames(t1$benefit_burden_ratio, t1$state)
  want <- c(`Nct Of Delhi` = 6.04, Karnataka = 1.59, Maharashtra = 1.34, `Tamil Nadu` = 0.51, `Rest of India` = 0.91)
  f4 <- t1[state == "Four use states"]
  msg("self-test vs run 3: ratios %s; four states pop %.0f%% facilities %.0f%% SO2 %.0f%% exposure %.0f%%",
      paste(sprintf("%.2f", r[names(want)]), collapse = " / "), 100 * f4$population_share, 100 * f4$facilities_share, 100 * f4$so2_t_share, 100 * f4$exposure_share)
  # Delhi is checked loosely: run 3 weighted by gridded population (person_ugm3 from 16d), the golden
  # file carries only the NFHS-frame population, which puts ~4% more exposure on Delhi (6.04 -> 5.83).
  tol <- c(`Nct Of Delhi` = 0.25, Karnataka = 0.02, Maharashtra = 0.02, `Tamil Nadu` = 0.02, `Rest of India` = 0.02)
  assert(all(abs(r[names(want)] - want) < tol) && abs(f4$exposure_share - 0.47) < 0.005 && abs(f4$so2_t_share - 0.69) < 0.006,
         "21 no longer reproduces the run-3 ledger")
}

# ---- this run ----------------------------------------------------------------------------------
inc <- fread(out("tab16_inmap_district.csv"))[, .(zone_uid, dpm25, person_ugm3_grid = person_ugm3)]
fac <- fread(out("dc_current_inventory.csv")); if ("is_operating" %in% names(fac) && DC_SCOPE == "operating") fac <- fac[is_operating == TRUE | is_operating == 1]
dd <- dist_of(path_frame, inc); dd[, person_ugm3 := fifelse(is.na(person_ugm3_grid), 0, person_ugm3_grid)]   # exposure on gridded population, as run 3
t <- ledger(dd, fac, fread(out("tab16b_emissions_by_state.csv")))
pct <- function(x) round(100 * x, 1)
print(t[, .(state, use = pct(use_share), pop = pct(population_share), facilities = pct(facilities_share), so2 = pct(so2_t_share),
            exposure = pct(exposure_share), ratio = round(benefit_burden_ratio, 2), use_pc = round(use_per_capita_index, 1))])
fwrite(cbind(scope = DC_SCOPE, inventory = INVENTORY, t), out("tab21_benefit_burden.csv"))
msg("21 done")
