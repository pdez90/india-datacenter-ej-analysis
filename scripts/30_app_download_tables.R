# =============================================================================
# 30_app_download_tables.R  —  two documented CSVs for the companion app's download buttons
# -----------------------------------------------------------------------------
# Written 4 Oct 2026 (Priyanka's request). Operating scope. Each file is self-describing: a `notes`
# column on every row, and a source/definition block in the file's companion _README.txt (also baked
# into the app's documented-dataset zip by 22).
#   1. outputs/public/power_interruptions_backup_diesel.csv
#      One row per hosting district: facilities and electricity, ESMI monitors and measured interruption
#      hours (all / urban / rural), the outage-hour tier the diesel model used and the value it took
#      (O_d), the resulting generator hours, diesel electricity and NOx / PM2.5 at the central scenario
#      (f = 0.4, 100 h testing), plus one total row (its notes give the scenario range).
#   2. outputs/public/emission_factors.csv
#      Every emission-related factor the analysis uses, one row per factor x level: state marginal
#      emission factors (Sengupta et al. 2022, flat-load weighting) for CO2, SO2, NOx and PM2.5; the
#      national-median imputation for states without a factor (none needed in v2); the average grid CO2 factor; PUE,
#      utilisation and water intensities; the gas:coal pollutant split and stack heights used to place
#      emissions at plants; the backup-diesel emission factors. With unit, where it applies, source,
#      how it is used and the script that uses it.
# Both are copied into india-datacentre-ej/data/ when that folder exists. Run 30 before 22 (22 packs these files into the zip).
# =============================================================================
source(file.path(Sys.getenv("DCEJ_ROOT", unset = (function(){a<-commandArgs(FALSE);f<-sub("^--file=","",grep("^--file=",a,value=TRUE));d<-if(length(f)) dirname(normalizePath(f[1])) else getwd();while(!file.exists(file.path(d,"scripts","00_config.R"))&&d!=dirname(d)) d<-dirname(d);d})()), "scripts", "00_config.R"))
assert(DC_SCOPE == "operating" && !is_v1, "30 runs for the v2 operating scope only")
dir_pubout <- file.path(dir_out, "public"); dir.create(dir_pubout, showWarnings = FALSE)
app_data <- file.path(ROOT, "india-datacentre-ej", "data")

# ---- 1. power interruptions and backup diesel --------------------------------------------------
dd <- fread(out("tab29_diesel_district.csv"))
es <- fread(out("tab28_esmi_district_outages.csv"))
sc <- fread(out("tab29_diesel_scenarios.csv"))
cen <- sc[abs(f - 0.4) < 1e-9 & abs(testing_h - 100) < 1e-9]
tier_text <- c(`1` = "district urban monitors (ESMI)", `2` = "district monitors, rural only (ESMI)",
               `3` = "state median of urban monitors (no monitor in the district)", `4` = "national median of urban monitors (no monitor in the district or state)")
p <- merge(dd, es[, .(zone_uid, n_rural_monitors, h_rural = interruption_h_per_yr_rural, min_monitor_h, max_monitor_h, recorded_monitor_days, first_date, last_date, esmi_districts_pooled)],
           by = "zone_uid", all.x = TRUE)
p <- p[order(-diesel_nox_t_central), .(
  district = dist_name, state = state_name,
  operating_facilities = facilities, electricity_gwh_per_yr = round(e_gwh, 1),
  esmi_monitors = fifelse(is.na(n_monitors), 0L, as.integer(n_monitors)), esmi_urban_monitors = fifelse(is.na(n_urban_monitors), 0L, as.integer(n_urban_monitors)),
  esmi_rural_monitors = fifelse(is.na(n_rural_monitors), 0L, as.integer(n_rural_monitors)),
  interruption_h_per_yr_all_monitors = round(h_all, 1), interruption_h_per_yr_urban = round(h_urban, 1), interruption_h_per_yr_rural = round(h_rural, 1),
  monitor_min_h = round(min_monitor_h, 1), monitor_max_h = round(max_monitor_h, 1), recorded_monitor_days, esmi_period_first = first_date, esmi_period_last = last_date,
  esmi_districts_pooled,
  state_urban_median_h = round(state_urban_median, 1), state_urban_monitors,
  outage_hours_tier = tier, outage_hours_tier_label = tier_text[as.character(tier)],
  outage_hours_used_O_d = round(O_d, 1), feeder_fraction_f = 0.4, testing_h_per_yr = 100,
  generator_hours_central = round(H_central, 1), diesel_electricity_mwh_central = round(diesel_mwh_central, 1),
  diesel_nox_t_central = round(diesel_nox_t_central, 2), diesel_pm25_t_central = round(diesel_pm25_t_central, 3),
  outage_hours_zero_fill_rule = round(O_zero, 1),
  notes = fifelse(tier == 1, "Measured in this district: ESMI urban monitors (state capital, district HQ or municipal area).",
          fifelse(tier == 2, "Measured in this district, but only rural (gram panchayat) monitors.",
          fifelse(tier == 3, "No ESMI monitor in this district: state median of urban monitors.",
                  "No ESMI monitor in this district or its state: national median of urban monitors."))))]
assert(nrow(p) == V2_HOST_OP && !anyNA(p$outage_hours_used_O_d), "one row per hosting district with an outage value")
tot <- data.table(district = "ALL HOSTING DISTRICTS", state = "", operating_facilities = sum(p$operating_facilities), electricity_gwh_per_yr = round(sum(p$electricity_gwh_per_yr), 1),
                  diesel_electricity_mwh_central = round(cen$diesel_mwh, 1), diesel_nox_t_central = round(cen$diesel_nox_t, 2), diesel_pm25_t_central = round(cen$diesel_pm25_t, 3),
                  notes = sprintf("Central scenario total: diesel NOx = %.2f%% of the fleet's grid-attributable marginal NOx (%.0f t); PM2.5 = %.2f%% of grid PM2.5. Across f in {0.2, 0.4, 1.0} x testing in {50, 100, 150} h: NOx %.1f-%.1f%%. Zero-fill rule (unmonitored districts counted at zero measured hours): %.2f%%.",
                                  cen$pct_grid_nox, cen$grid_nox_t, cen$pct_grid_pm25, min(sc$pct_grid_nox), max(sc$pct_grid_nox), cen$pct_grid_nox_zerofill))
p <- rbind(p, tot, fill = TRUE)
f1 <- file.path(dir_pubout, "power_interruptions_backup_diesel.csv"); fwrite(p, f1)
readme1 <- c(
  "power_interruptions_backup_diesel.csv - one row per district hosting an operating data centre (India, inventory of 30 September 2026), plus a total row.",
  "",
  "Interruption hours. Prayas Energy Group, Electricity Supply Monitoring Initiative (ESMI), Harvard Dataverse doi:10.7910/DVN/CLLZZM:",
  "voltage logged every minute at customer connections, Oct 2014 - Dec 2019. A minute with voltage below 80 V is an interruption; a blank",
  "minute is missing data, not an interruption. Hours per year = interrupted minutes / recorded minutes x 8760, per monitor; monitors with",
  "fewer than 30 recorded days are excluded; the district value is the plain mean over its monitors. Urban = state capital, district",
  "headquarters or other municipal area; rural = gram panchayat. Monitors are at ordinary customer premises, not at data centres.",
  "ESMI terms: non-commercial research use with acknowledgement; Prayas asks to be informed of publications (esmi@prayaspune.org).",
  "",
  "Backup diesel (central scenario). Generator hours H = f x O_d + T, with O_d the district's urban-monitor interruption hours",
  "(tiers: district urban monitors; district rural-only monitors, none in practice; state median of urban monitors; national median of urban monitors),",
  "f = 0.4 (data centres sit on dedicated high-tension feeders that see fewer interruptions than customer connections; 0.2-1.0 in the",
  "sensitivity), T = 100 h/yr of scheduled testing (50-150). Diesel electricity = facility average load x H (full load during backup).",
  "Emission factors: NOx 8 g/kWh (4-14), PM2.5 0.20 g/kWh (0.05-0.5), SO2 0.005 g/kWh, CO2 0.75 kg/kWh. The zero-fill alternative",
  "(unmonitored districts get testing hours only) is reported as outage_hours_zero_fill_rule.",
  "Scripts: 28_esmi_outages.py, 29_backup_diesel.R, 30_app_download_tables.R. Licence of this table: CC BY-NC 4.0.")
writeLines(readme1, sub("\\.csv$", "_README.txt", f1))

# ---- 2. emission factors ----------------------------------------------------------------------
gp  <- fread(path_grid_params)
mef <- fread(file.path(dir_data, "sengupta_mef_flatload.csv"))
hs  <- fread(out("tab15b_s43_hosting_factors.csv"))
# state-name spellings differ between Sengupta ("Delhi", "Chhattisgarh", "Uttarakhand") and the 2015 layer
# ("Nct Of Delhi", "Chattisgarh", "Uttarakhaand"); match on the same normalised key as 15b.
norm <- function(s) { s <- tolower(gsub("[^A-Za-z]", "", s)); s <- sub("^nctof", "", s); s <- sub("^chhattisgarh$", "chattisgarh", s)
                      s <- sub("^uttarakhaand$", "uttarakhand", s); s <- sub("^orissa$", "odisha", s); s <- sub("^uttaranchal$", "uttarakhand", s); s }
hs[, key := norm(load_state_2015)]
assert(all(hs$key %in% norm(mef$state)), "30: hosting load states not all found in the Sengupta factor table: %s", paste(hs[!key %in% norm(mef$state), load_state_2015], collapse = ", "))
imp_f <- out("tab15b_mef_imputed_states.csv"); imp <- if (file.exists(imp_f)) fread(imp_f) else NULL
mb  <- fread(out("tab15b_marginal.csv"))
load_states <- unique(mb$load_state_2015)
row <- function(...) data.table(...)
E <- list()
# 2a. state marginal emission factors (flat load)
src_mef <- "Sengupta, Adams, Deetjen, Kamboj, Tongia, Azevedo (2022) marginal emission factors for Indian power generation, 2017-18 simulation; KiltHub doi:10.1184/R1/19359989 (CC BY 4.0)"
how_mef <- "Annual flat-load factor: five daily periods weighted by duration (morning peak, midday, afternoon, evening peak 4 h each; overnight 8 h) and seasons by duration (dry 7/12, monsoon 5/12). Applied to each facility's central-scenario electricity by the state it draws load from."
for (i in seq_len(nrow(mef))) for (pl in c("co2", "so2", "nox", "pm25")) {
  st <- mef$state[i]; used <- norm(st) %in% hs$key
  E[[length(E) + 1]] <- row(factor = sprintf("Marginal emission factor, %s", toupper(sub("pm25", "PM2.5", pl))), category = "grid, marginal (pollutants and marginal CO2)",
    applies_to = st, level = "central", value = signif(mef[[pl]][i], 5), unit = "kg per MWh of electricity",
    used_in_paper = fifelse(used, {n_f <- hs[key == norm(st), facilities][1]; sprintf("yes: %d operating %s load in this state", n_f, if (isTRUE(n_f == 1)) "facility draws" else "facilities draw")}, "no operating facility draws load in this state"),
    source = src_mef, how_used = how_mef, script = "15a_mef_kilthub.R -> 15b_marginal_mef.R", notes = "")
}
if (!is.null(imp) && nrow(imp)) for (i in seq_len(nrow(imp))) for (pl in c("co2", "so2", "nox", "pm25")) if (pl %in% names(imp))
  E[[length(E) + 1]] <- row(factor = sprintf("Marginal emission factor, %s (imputed)", toupper(sub("pm25", "PM2.5", pl))), category = "grid, marginal (pollutants and marginal CO2)",
    applies_to = imp$state[i], level = "central", value = signif(imp[[pl]][i], 5), unit = "kg per MWh", used_in_paper = "yes",
    source = "national facility-weighted median of the Sengupta et al. state factors", how_used = "The state has no factor of its own (Chandigarh, Puducherry); its facilities' electricity (< 5 % of the total) takes the national median.",
    script = "15b_marginal_mef.R", notes = "")
# 2b. grid-level parameters from india_grid_params.csv
gpn <- c(grid_co2_t_per_mwh = "Average grid CO2 factor", pue = "Power usage effectiveness (PUE)", utilization = "IT utilisation",
         wue_l_per_kwh_it = "On-site water use effectiveness (scope 1)", grid_water_l_per_kwh = "Grid water consumption (scope 2, with hydropower evaporation)",
         grid_water_nohydro_l_per_kwh = "Grid water consumption (scope 2, hydropower evaporation excluded)", coal_share_generation = "Coal share of national generation",
         mef_tiermedian_t_per_mwh = "Tier-median marginal CO2 factor (cross-check only)")
for (k in names(gpn)) { g <- gp[param == k]; if (!nrow(g)) next
  for (lv in c("low", "central", "high")) E[[length(E) + 1]] <- row(factor = gpn[[k]], category = fifelse(grepl("water|PUE|utilisation", gpn[[k]]), "energy and water intensity", "grid, average"),
    applies_to = "all facilities (India)", level = lv, value = g[[lv]], unit = g$unit, used_in_paper = fifelse(k == "mef_tiermedian_t_per_mwh", "cross-check only (not a reported result)", "yes"),
    source = g$source, how_used = fcase(k == "grid_co2_t_per_mwh", "Electricity x factor = average-grid CO2 footprint (the marginal CO2 uses the state factors above).",
                                        k == "pue", "Electricity = IT MW x PUE x utilisation x 8.76 GWh/yr.", k == "utilization", "Electricity = IT MW x PUE x utilisation x 8.76 GWh/yr.",
                                        k == "wue_l_per_kwh_it", "Scope-1 water = IT electricity x WUE.", grepl("grid_water", k), "Scope-2 water = facility electricity x factor.",
                                        k == "coal_share_generation", "Coal-attributable share of the sector's electricity.", default = "See india_grid_params.csv"),
    script = "15_attribution.R", notes = g$provenance) }
# 2c. placement of emissions at plants
for (x in list(list("Gas:coal emission ratio, SO2", 1 / 5750), list("Gas:coal emission ratio, NOx", 1 / 57.5), list("Gas:coal emission ratio, PM2.5", 1 / 230)))
  E[[length(E) + 1]] <- row(factor = x[[1]], category = "placement of grid emissions at plants", applies_to = "gas vs coal plants, national", level = "central", value = signif(x[[2]], 4), unit = "ratio (gas / coal) per MWh",
    used_in_paper = "yes", source = "fixed national ratios used in the dispersion set-up (SI S4.1)", how_used = "Each load state's marginal tonnes are split between its coal and gas plants in this ratio (gas fraction = r/(1+r)), then allocated to plants by nameplate capacity; states with no plant of a fuel pass that share to all plants of that fuel nationally.", script = "16b_build_emissions.R", notes = "Gas carries 0.017 % of SO2, 1.7 % of NOx and 0.43 % of PM2.5.")
for (x in list(list("Stack height, coal plants", 200), list("Stack height, gas plants", 100)))
  E[[length(E) + 1]] <- row(factor = x[[1]], category = "placement of grid emissions at plants", applies_to = "all plants of the fuel", level = "central", value = x[[2]], unit = "m",
    used_in_paper = "yes", source = "Peshin et al. (2024)", how_used = "Release height in InMAP; no stack diameter, temperature or exit velocity, so plume rise is zero.", script = "16b_build_emissions.R", notes = "")
# 2d. backup diesel
for (x in list(list("Backup diesel NOx", 4, 8, 14, "g/kWh", "CPCB (2014) genset norms; U.S. EPA AP-42 uncontrolled diesel engines"),
               list("Backup diesel PM2.5", 0.05, 0.20, 0.5, "g/kWh", "AP-42 / CPCB range"),
               list("Backup diesel SO2", 0.005, 0.005, 0.005, "g/kWh", "BS-VI diesel, 10 ppm sulphur"),
               list("Backup diesel CO2", 0.75, 0.75, 0.75, "kg/kWh", "diesel combustion, ~0.75 kg CO2 per kWh generated")))
  for (lv in c("low", "central", "high")) E[[length(E) + 1]] <- row(factor = x[[1]], category = "backup diesel generators (scope 1)", applies_to = "all facilities", level = lv,
    value = x[[c(low = 2, central = 3, high = 4)[[lv]]]], unit = x[[5]], used_in_paper = fifelse(grepl("NOx|PM", x[[1]]), "yes (range reported)", "yes (central only)"), source = x[[6]],
    how_used = "Diesel electricity (facility average load x generator hours; hours = 0.4 x district interruption hours + 100 h testing) x factor.", script = "29_backup_diesel.R", notes = "")
ef <- rbindlist(E, fill = TRUE)
f2 <- file.path(dir_pubout, "emission_factors.csv"); fwrite(ef, f2)
readme2 <- c(
  "emission_factors.csv - every emission-related factor used in the analysis, one row per factor and level (low / central / high where a range is used).",
  "Columns: factor, category, applies_to (state, fuel or all facilities), level, value, unit, used_in_paper, source, how_used, script, notes.",
  "Grid marginal factors (kg/MWh) are Sengupta et al. (2022), 2017-18, flat-load weighted; each facility draws load from its own state (Telangana",
  "has its own row; every load state of the 233 operating facilities has a factor, so no imputation was needed). The average grid CO2 factor (CEA 2025, FY2024-25) gives the",
  "footprint CO2; the marginal factors give the marginal CO2 and all SO2 / NOx / PM2.5. Electricity = IT MW x PUE x utilisation x 8.76.",
  "Scripts: 15_attribution.R, 15a_mef_kilthub.R, 15b_marginal_mef.R, 16b_build_emissions.R, 29_backup_diesel.R. Licence: CC BY-NC 4.0.")
writeLines(readme2, sub("\\.csv$", "_README.txt", f2))
msg("wrote %s (%d rows) and %s (%d rows)", basename(f1), nrow(p), basename(f2), nrow(ef))
if (dir.exists(app_data)) { for (f in c(f1, sub("\\.csv$", "_README.txt", f1), f2, sub("\\.csv$", "_README.txt", f2))) file.copy(f, file.path(app_data, basename(f)), overwrite = TRUE); msg("copied to %s", app_data) }
