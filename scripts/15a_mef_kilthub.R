# =============================================================================
# 15a_mef_kilthub.R  —  Sengupta et al. (2022) marginal emission factors
# -----------------------------------------------------------------------------
# Source: KiltHub (Carnegie Mellon), "Marginal Emission Factors for Indian Power
# Generation", DOI 10.1184/R1/19359989, CC BY 4.0: CSVs by state x season x
# time-of-day period for CO2, SO2, NOx and primary PM2.5 (2017-18 all states;
# 2030-31 TERI scenario), plus a README. Built from the CSVs, never the PDF (the
# published SI tables had a typesetting erratum: 2017-18 monsoon NOx = SO2).
#
# Flat-load annual factor for a continuous load (SI S4 ¶132): the five daily
# periods weighted by duration (morning peak, midday, afternoon, evening peak
# 4 h each; overnight 8 h) and the two seasons by duration (dry 7/12,
# monsoon 5/12).
#
# Writes:
#   data/raw/mef_kilthub/*            the downloaded files (archival copy)
#   data/sengupta_mef_long.csv        long table (state, pollutant, season, period, value)
#   data/sengupta_mef_flatload.csv    one row per state, kg/MWh; read by 15b
# If the download or parsing fails, 15b falls back to the rounded SI Table S4.3
# (data/sengupta_mef_flatload_tableS43.csv), which reproduces the published
# totals to within 0.5% (CO2 14.65 Mt exact).
# =============================================================================
source(file.path(Sys.getenv("DCEJ_ROOT", unset = (function(){a<-commandArgs(FALSE);f<-sub("^--file=","",grep("^--file=",a,value=TRUE));d<-if(length(f)) dirname(normalizePath(f[1])) else getwd();while(!file.exists(file.path(d,"scripts","00_config.R"))&&d!=dirname(d)) d<-dirname(d);d})()), "scripts", "00_config.R"))

kdir <- file.path(dir_data, "raw", "mef_kilthub"); dir.create(kdir, recursive = TRUE, showWarnings = FALSE)
if (!length(list.files(kdir, pattern = "\\.csv$", recursive = TRUE))) {
  meta <- jsonlite::fromJSON("https://api.figshare.com/v2/articles/19359989")
  for (i in seq_len(nrow(meta$files))) {
    msg("downloading %s", meta$files$name[i])
    download.file(meta$files$download_url[i], file.path(kdir, meta$files$name[i]), mode = "wb", quiet = TRUE)
  }
  zips <- list.files(kdir, pattern = "\\.zip$", full.names = TRUE)
  for (z in zips) unzip(z, exdir = kdir)
}
files <- list.files(kdir, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
msg("KiltHub CSVs: %d", length(files))
# (file heads are no longer printed; the layout is documented below)

# ---- parse to a long table ---------------------------------------------------------------
# KiltHub layout (README.txt): <POL>_MEF_all_states_long.csv for the 2017-18 simulation with
# columns sim.state, period (morn_peak, mid_day, afternoon, evn_peak, overnight), monsoon
# (TRUE = Jun-Oct), MEF_<POL>_reg (kg/MWh), _se, _r2. The *_TERI_* files are the 2030-31
# scenario and are not used. NO files hold NOx.
state_names <- c(AP = "Andhra Pradesh", ARP = "Arunachal Pradesh", Chhatisgarh = "Chhattisgarh", HP = "Himachal Pradesh",
                 JK = "Jammu & Kashmir", MP = "Madhya Pradesh", TN = "Tamil Nadu", UP = "Uttar Pradesh", WB = "West Bengal")
pols <- c(CO2 = "CO2", SO2 = "SO2", NOX = "NO", PM25 = "PM25")
long <- rbindlist(lapply(names(pols), function(p) {
  f <- files[basename(files) == sprintf("%s_MEF_all_states_long.csv", pols[[p]])]
  assert(length(f) == 1, "missing %s_MEF_all_states_long.csv", pols[[p]])
  d <- fread(f); v <- grep("_reg$", names(d), value = TRUE)
  d[, .(state = fifelse(sim.state %in% names(state_names), state_names[sim.state], sim.state), state_code = sim.state,
        pollutant = p, season = fifelse(monsoon, "monsoon", "dry"), period, value = get(v), se = get(paste0(v, "_se")))]
}))
assert(long[, uniqueN(period)] == 5 && long[, uniqueN(season)] == 2, "unexpected period/season structure")
fwrite(long, file.path(dir_data, "sengupta_mef_long.csv"))
msg("long table: %d rows (%d states x 4 pollutants x 2 seasons x 5 periods)", nrow(long), uniqueN(long$state))
assert(long[pollutant == "NOX" & season == "monsoon", sum(value, na.rm = TRUE)] != long[pollutant == "SO2" & season == "monsoon", sum(value, na.rm = TRUE)],
       "monsoon NOx equals monsoon SO2 - this is the published SI erratum; use the KiltHub CSVs")

# ---- flat-load annual factors ----------------------------------------------------------------
period_hours <- function(p) fifelse(grepl("night", p), 8, 4)
long[, w := period_hours(period) * fifelse(season == "monsoon", 5 / 12, 7 / 12)]
flat <- dcast(long[, .(v = sum(value * w) / sum(w)), by = .(state, pollutant)], state ~ pollutant, value.var = "v")
setnames(flat, c("CO2", "SO2", "NOX", "PM25"), c("co2", "so2", "nox", "pm25"))
# check against the rounded SI Table S4.3
s43 <- fread(file.path(dir_data, "sengupta_mef_flatload_tableS43.csv"))
s43[, state := fifelse(state_2015 == "Nct Of Delhi", "Delhi", fifelse(state_2015 == "Chattisgarh", "Chhattisgarh", state_2015))]
m <- merge(flat, s43, by = "state")
assert(nrow(m) == 17 && m[, max(abs(round(co2) - co2_kg_mwh))] <= 1 && m[, max(abs(round(so2, 1) - so2_kg_mwh))] <= 0.1 + 1e-9,
       "flat-load factors do not reproduce SI Table S4.3")
msg("flat-load factors reproduce SI Table S4.3 for all 17 hosting states")
fwrite(flat, file.path(dir_data, "sengupta_mef_flatload.csv"))
print(flat)
