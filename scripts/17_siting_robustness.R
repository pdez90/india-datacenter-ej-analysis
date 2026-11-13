# =============================================================================
# 17_siting_robustness.R  —  additional siting robustness (SI S10) and the confidence sensitivity (SI S1)
# -----------------------------------------------------------------------------
# Rebuilt 3 Oct 2026 (the original 17 was lost on 29 Sep). Operating scope.
#  A. Intensity vs presence (tab17b): among HOSTING districts only, outcome ~ log(facility count)
#     [or log(allocated IT MW)] + urban share + nighttime lights + state FE; HC1 standard errors.
#     Outcomes: wealth, SC/ST, BPL, ACAG PM2.5, surface NO2.
#  B. Metropolitan fixed effects (tab17c): districts whose centroid lies within 100 km of one of the
#     seven largest metropolitan centres (Delhi, Mumbai, Bengaluru, Chennai, Hyderabad, Kolkata, Pune),
#     each assigned to its nearest centre; outcome ~ hosting + metro FE, HC1 standard errors.
#  C. Connectivity (tab17d): log distance (km) from each district centroid to the nearest internet
#     exchange point. Exchanges: PeeringDB, all Indian IXPs with status ok, retrieved 3 Oct 2026
#     (data/ixp_india_peeringdb_20261003.csv; 42 exchanges in 13 cities; city-centre coordinates).
#     LPM: hosting ~ log distance + full SES set + state FE (as 07's P(hosting) model), with and
#     without the distance term, to show how much of the wealth coefficient it absorbs.
#  E. Confidence sensitivity (tab17e): replaces the v1 "two-source" sensitivity (whose source counts
#     were lost). Hosting is redefined as hosting at least one HIGH-confidence operating facility
#     (outputs/public/india_datacentres_public.csv, column confidence; 24) and the all-India
#     standardized differences of Table S2.1 are recomputed.
# Outputs: tab17b_intensity.csv, tab17c_metro_fe.csv, tab17d_ixp.csv, tab17e_confidence_sensitivity.csv
# =============================================================================
source(file.path(Sys.getenv("DCEJ_ROOT", unset = (function(){a<-commandArgs(FALSE);f<-sub("^--file=","",grep("^--file=",a,value=TRUE));d<-if(length(f)) dirname(normalizePath(f[1])) else getwd();while(!file.exists(file.path(d,"scripts","00_config.R"))&&d!=dirname(d)) d<-dirname(d);d})()), "scripts", "00_config.R"))
assert(DC_SCOPE == "operating", "17 runs for the operating scope only")

METRO_KM <- 100
METROS <- data.table(metro = c("Delhi", "Mumbai", "Bengaluru", "Chennai", "Hyderabad", "Kolkata", "Pune"),
                     lat = c(28.6139, 19.0760, 12.9716, 13.0827, 17.3850, 22.5726, 18.5204),
                     lon = c(77.2090, 72.8777, 77.5946, 80.2707, 78.4867, 88.3639, 73.8567))
OUTCOMES <- c(wealth_mean_2019 = "Mean wealth score", share_scst_2019 = "Share SC/ST", share_bpl_2019 = "Share BPL card",
              pm25_acag_local = "PM2.5, ACAG (ug/m3)", no2_surf = "Surface NO2 (ppb)")

fr <- fread(path_frame); fr[, st := state_name_2015]
cen <- st_coordinates(st_transform(st_point_on_surface(st_geometry(read_districts())), 4326))
fr <- merge(fr, data.table(zone_uid = read_districts()$zone_uid, lon = cen[, 1], lat = cen[, 2]), by = "zone_uid")
hav <- function(lat1, lon1, lat2, lon2) { r <- pi / 180; a <- sin((lat2 - lat1) * r / 2)^2 + cos(lat1 * r) * cos(lat2 * r) * sin((lon2 - lon1) * r / 2)^2; 2 * 6371 * asin(sqrt(a)) }

hc1 <- function(fit) {             # heteroskedasticity-robust (HC1) standard errors, no package dependency
  X <- model.matrix(fit); X <- X[, !is.na(coef(fit)), drop = FALSE]; e <- residuals(fit); n <- nrow(X); k <- ncol(X)
  B <- summary(fit)$cov.unscaled; M <- t(X * e) %*% (X * e); sqrt(diag(B %*% M %*% B) * n / (n - k))
}
row_of <- function(fit, term, label, extra = list()) {
  b <- coef(fit)[[term]]; se <- hc1(fit)[[term]]; df <- fit$df.residual
  as.data.table(c(list(model = label, term = term, estimate = b, se_hc1 = se, p = 2 * pt(-abs(b / se), df), n = nobs(fit)), extra))
}

# ---- A. intensity ----------------------------------------------------------------------------
fx <- fread(out("tab15_facilities.csv"))
mw <- fx[, .(mw_it = sum(mw_it, na.rm = TRUE)), by = zone_uid]
h <- merge(fr[hosting == 1], mw, by = "zone_uid", all.x = TRUE)
h[, `:=`(log_count = log(dc_count), log_mw = log(pmax(mw_it, 0.1)))]
tA <- rbindlist(lapply(names(OUTCOMES), function(y) rbindlist(lapply(c("log_count", "log_mw"), function(x) {
  d <- h[complete.cases(h[, c(y, x, "urban_share_2019", "ntl_mean"), with = FALSE])]
  fit <- lm(as.formula(sprintf("%s ~ %s + urban_share_2019 + ntl_mean + factor(st)", y, x)), data = d)
  row_of(fit, x, OUTCOMES[[y]]) }))))
fwrite(tA, out("tab17b_intensity.csv")); print(tA[, .(model, term, est = signif(estimate, 3), se = signif(se_hc1, 3), p = round(p, 3), n)])

# ---- B. metro fixed effects ----------------------------------------------------------------------
dm <- sapply(seq_len(nrow(METROS)), function(i) hav(fr$lat, fr$lon, METROS$lat[i], METROS$lon[i]))
fr[, `:=`(metro = METROS$metro[max.col(-dm)], metro_km = apply(dm, 1, min))]
mt <- fr[metro_km <= METRO_KM]
msg("metro sample: %d districts within %d km of the seven metros (%d hosting)", nrow(mt), METRO_KM, mt[hosting == 1, .N])
tB <- rbindlist(lapply(names(OUTCOMES), function(y) {
  d <- mt[!is.na(get(y))]; fit <- lm(as.formula(sprintf("%s ~ hosting + factor(metro)", y)), data = d)
  row_of(fit, "hosting", OUTCOMES[[y]], list(districts_in_sample = nrow(mt), hosting_in_sample = mt[hosting == 1, .N])) }))
fwrite(tB, out("tab17c_metro_fe.csv")); print(tB[, .(model, est = signif(estimate, 3), se = signif(se_hc1, 3), p = round(p, 3), n)])

# ---- C. connectivity (internet exchange points) --------------------------------------------------
ixp <- fread(file.path(dir_data, "ixp_india_peeringdb_20261003.csv"))
ix <- unique(ixp[, .(city, lat, lon)])
di <- sapply(seq_len(nrow(ix)), function(i) hav(fr$lat, fr$lon, ix$lat[i], ix$lon[i]))
fr[, ixp_km := apply(di, 1, min)][, log_ixp_km := log(pmax(ixp_km, 1))]
SES <- "urban_share_2019 + ntl_mean + wealth_mean_2019 + share_scst_2019 + share_muslim_2019 + share_bpl_2019 + factor(st)"
ph <- fr[complete.cases(fr[, .(hosting, urban_share_2019, ntl_mean, wealth_mean_2019, share_scst_2019, share_muslim_2019, share_bpl_2019, log_ixp_km)])]
f0 <- lm(as.formula(paste("hosting ~", SES)), data = ph)
f1 <- lm(as.formula(paste("hosting ~ log_ixp_km +", SES)), data = ph)
w0 <- lm(hosting ~ wealth_mean_2019 + factor(st), data = ph)                   # wealth alone, state FE
w1 <- lm(hosting ~ wealth_mean_2019 + log_ixp_km + factor(st), data = ph)
tC <- rbind(row_of(f1, "log_ixp_km", "LPM, full SES + state FE: log km to nearest IXP"),
            row_of(f0, "wealth_mean_2019", "LPM, full SES + state FE: wealth (without IXP)"),
            row_of(f1, "wealth_mean_2019", "LPM, full SES + state FE: wealth (with IXP)"),
            row_of(w0, "wealth_mean_2019", "LPM, wealth + state FE: wealth (without IXP)"),
            row_of(w1, "wealth_mean_2019", "LPM, wealth + state FE: wealth (with IXP)"),
            row_of(w1, "log_ixp_km", "LPM, wealth + state FE: log km to nearest IXP"))
tC[, `:=`(ixp_exchanges = nrow(ixp), ixp_cities = nrow(ix), hosting_median_ixp_km = fr[hosting == 1, median(ixp_km)], nonhosting_median_ixp_km = fr[hosting == 0, median(ixp_km)])]
fwrite(tC, out("tab17d_ixp.csv")); print(tC[, .(model, est = signif(estimate, 3), se = signif(se_hc1, 3), p = round(p, 3))])
msg("wealth coefficient falls by %.0f%% when the IXP distance is added (wealth + state FE model)",
    100 * (1 - coef(w1)[["wealth_mean_2019"]] / coef(w0)[["wealth_mean_2019"]]))

# ---- E. high-confidence sensitivity --------------------------------------------------------------------
pub_f <- file.path(ROOT, "outputs", "public", "india_datacentres_public.csv")
if (!file.exists(pub_f)) { msg("E skipped: run 24 first"); quit(save = "no") }
pub <- fread(pub_f)[status == "operating"]
inv <- fread(out("dc_current_inventory.csv"))[, .(facility_id, zone_uid)]
hi <- merge(pub[confidence == "high", .(facility_id)], inv, by = "facility_id")
fr[, hosting_high := as.integer(zone_uid %in% hi$zone_uid)]
VARS <- c(urban_share_2019 = "Urban share", wealth_mean_2019 = "Mean wealth score", rwi_mean = "Relative Wealth Index",
          share_sc_2019 = "Share Scheduled Caste", share_st_2019 = "Share Scheduled Tribe", share_scst_2019 = "Share SC/ST",
          share_obc_2019 = "Share OBC", share_muslim_2019 = "Share Muslim", share_bpl_2019 = "Share BPL card",
          share_no_electricity_2019 = "Share without electricity", pm25_acag_local = "PM2.5, ACAG", pm25_lghap = "PM2.5, LGHAP",
          no2_surf = "Surface NO2", o3_summer = "Surface ozone, summer", hot_days_peryr_era5 = "Hot days >= 35 C")
VARS <- VARS[names(VARS) %in% names(fr)]
dstd <- function(a, b) { a <- a[!is.na(a)]; b <- b[!is.na(b)]; (mean(a) - mean(b)) / sqrt((var(a) + var(b)) / 2) }
tE <- rbindlist(lapply(names(VARS), function(v) data.table(variable = VARS[[v]],
  d_all_hosting = dstd(fr[hosting == 1][[v]], fr[hosting == 0][[v]]),
  d_high_confidence_hosting = dstd(fr[hosting_high == 1][[v]], fr[hosting_high == 0][[v]]))))
tE[, `:=`(n_hosting = fr[, sum(hosting)], n_hosting_high = fr[, sum(hosting_high)], high_conf_facilities = nrow(hi))]
tE[, same_sign := sign(d_all_hosting) == sign(d_high_confidence_hosting)]
fwrite(tE, out("tab17e_confidence_sensitivity.csv"))
print(tE[, .(variable, all = round(d_all_hosting, 2), high = round(d_high_confidence_hosting, 2), same_sign)])
msg("confidence sensitivity: %d of %d hosting districts host a high-confidence facility (%d facilities)", fr[, sum(hosting_high)], fr[, sum(hosting)], nrow(hi))
msg("17 done")
