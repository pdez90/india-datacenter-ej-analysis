# =============================================================================
# 07_siting.R  —  where are the facilities? The sequence of comparisons (i)-(v)
# -----------------------------------------------------------------------------
# Manuscript section 2.4 and SI sections S2-S3 (Tables S2.1, S2.2, S2.3; S3 text).
#  (i)  all-India: hosting vs non-hosting districts, standardized difference
#       d = (m1 - m0) / sqrt((v1 + v0) / 2)                       -> Table S2.1
#  (ii) within-state: variables demeaned within the 25 hosting states (2015
#       state labels, Telangana inside Andhra Pradesh); d as above; Welch p
#  (iii) urban-only: hosting states, districts with NFHS urban share >= 0.30,
#       demeaned within state; d and Welch p                      -> Table S2.2
#  (iv) matched pairs: each hosting district's K = 2 nearest non-hosting
#       districts in the same state on (urban share, log1p nighttime lights),
#       both standardized WITHIN STATE; caliper 0.75 SD (and a no-caliper
#       sensitivity); paired t on hosting minus mean-of-matches (descriptive,
#       controls re-used)                                          -> Table S2.3
#  (v)  regressions: outcome ~ hosting + urban share + nighttime lights (levels)
#       + wealth + SC/ST + Muslim + BPL + state FE; conventional and Conley
#       (200 km, uniform kernel) standard errors; PM2.5 re-estimated with
#       inverse-squared ACAG uncertainty weights                   -> SI S3 text
# Recovered 30 Sep 2026 against the SI. (i)-(iii) reproduce Tables S2.1/S2.2 to
# rounding (residual: Rangareddy wealth). (iv) reproduces 14 of 31 matched and a
# forced-match median distance of 1.18 SD; paired differences agree in sign and
# significance but differ by ~10-20% in size because the VIIRS product version
# used originally is not recorded (here: NOAA/VIIRS/DNB/ANNUAL_V22 'average',
# 2019-2024). (v) reproduces the null hosting coefficients (PM2.5 +0.05 vs
# published +0.18; NO2 +0.03 vs +0.05; summer ozone -1.2 vs -1.13).
# =============================================================================
source(file.path(Sys.getenv("DCEJ_ROOT", unset = (function(){a<-commandArgs(FALSE);f<-sub("^--file=","",grep("^--file=",a,value=TRUE));d<-if(length(f)) dirname(normalizePath(f[1])) else getwd();while(!file.exists(file.path(d,"scripts","00_config.R"))&&d!=dirname(d)) d<-dirname(d);d})()), "scripts", "00_config.R"))
MATCH_CALIPER <- 0.75; MATCH_K <- 2; URBAN_CUT <- 0.30; CONLEY_KM <- 200

fr <- fread(path_frame)
if (DC_SCOPE == "all") fr[, hosting := hosting_all]
fr[, st := state_name_2015]
hs <- fr[hosting == 1, unique(st)]
msg("%d hosting districts in %d states (2015 labels)", fr[hosting == 1, .N], length(hs))

VARS <- c(share_sc_2019 = "Share Scheduled Caste", share_st_2019 = "Share Scheduled Tribe", share_scst_2019 = "Share SC/ST",
          share_obc_2019 = "Share OBC", share_muslim_2019 = "Share Muslim", share_bpl_2019 = "Share BPL card",
          share_no_electricity_2019 = "Share without electricity", wealth_mean_2019 = "Mean wealth score",
          urban_share_2019 = "Urban share", rwi_mean = "Relative Wealth Index", pm25_lghap = "PM2.5, LGHAP",
          pm25_acag_local = "PM2.5, ACAG local", no2_surf = "Surface NO2 (1 km annual)", o3_annual = "Surface ozone, annual mean",
          o3_summer = "Surface ozone, summer", no2_column = "NO2 column, S5P", hcho_column = "HCHO column, S5P",
          t2m_mean_c = "Mean air temperature (ERA5-Land)", hot_days_peryr_era5 = "Hot days >= 35 C (ERA5-Land)",
          lst_day_c = "Daytime land-surface temperature (MODIS)")
VARS <- VARS[names(VARS) %in% names(fr)]
dstd <- function(a, b) { a <- a[!is.na(a)]; b <- b[!is.na(b)]; (mean(a) - mean(b)) / sqrt((var(a) + var(b)) / 2) }
wp <- function(a, b) tryCatch(t.test(a, b)$p.value, error = function(e) NA_real_)
demean <- function(d, v) d[[v]] - ave(d[[v]], d$st, FUN = function(z) mean(z, na.rm = TRUE))

# ---- (i) + (ii) ------------------------------------------------------------------------
g <- fr[st %in% hs]
t1 <- rbindlist(lapply(names(VARS), function(v) {
  x <- fr[[v]]; h <- fr$hosting == 1; dm <- demean(g, v)
  data.table(variable = VARS[[v]], hosting_mean = mean(x[h], na.rm = TRUE), nonhosting_mean = mean(x[!h], na.rm = TRUE),
             d_all_india = dstd(x[h], x[!h]), d_within_state = dstd(dm[g$hosting == 1], dm[g$hosting == 0]),
             p_within_state = wp(dm[g$hosting == 1], dm[g$hosting == 0]),
             diff_within_state = mean(dm[g$hosting == 1], na.rm = TRUE) - mean(dm[g$hosting == 0], na.rm = TRUE)) }))
fwrite(t1, out("tab1_dc_vs_nodc_profile.csv")); print(t1[, .(variable, hosting_mean, nonhosting_mean, d_all = round(d_all_india, 2), d_within = round(d_within_state, 2), p = signif(p_within_state, 2))])

# ---- (iii) urban-only --------------------------------------------------------------------
u <- fr[st %in% hs & urban_share_2019 >= URBAN_CUT]
t2 <- rbindlist(lapply(names(VARS), function(v) { dm <- demean(u, v); a <- dm[u$hosting == 1]; b <- dm[u$hosting == 0]
  data.table(variable = VARS[[v]], difference = mean(a, na.rm = TRUE) - mean(b, na.rm = TRUE), d = dstd(a, b), p = wp(a, b)) }))
t2[, `:=`(n_hosting = u[hosting == 1, .N], n_urban_nonhosting = u[hosting == 0, .N])]
fwrite(t2, out("tab1c_within_state_urban_profile.csv"))
msg("urban-only: %d hosting vs %d urban non-hosting districts", u[hosting == 1, .N], u[hosting == 0, .N])

# ---- (iv) matched pairs ------------------------------------------------------------------
m0 <- fr[st %in% hs & !is.na(urban_share_2019) & !is.na(ntl_mean)]
m0[, lntl := log1p(ntl_mean)]
m0[, `:=`(zu = (urban_share_2019 - mean(urban_share_2019)) / sd(urban_share_2019), zl = (lntl - mean(lntl)) / sd(lntl)), by = st]
match_pairs <- function(cal) rbindlist(lapply(which(m0$hosting == 1), function(i) {
  c <- m0[hosting == 0 & st == m0$st[i]]; if (!nrow(c)) return(NULL)
  d <- sqrt((c$zu - m0$zu[i])^2 + (c$zl - m0$zl[i])^2); o <- order(d)[seq_len(min(MATCH_K, nrow(c)))]; o <- o[d[o] <= cal]
  if (!length(o)) return(NULL); data.table(host = m0$zone_uid[i], host_name = m0$dist_name[i], ctrl = c$zone_uid[o], ctrl_name = c$dist_name[o], dist = d[o]) }))
paired <- function(p) rbindlist(lapply(names(VARS), function(v) {
  hv <- m0[match(unique(p$host), zone_uid)][[v]]; cm <- p[, .(m = mean(m0[[v]][match(ctrl, m0$zone_uid)], na.rm = TRUE)), by = host]$m
  data.table(variable = VARS[[v]], hosting_mean = mean(hv, na.rm = TRUE), matched_mean = mean(cm, na.rm = TRUE),
             paired_difference = mean(hv - cm, na.rm = TRUE), p_paired_descriptive = tryCatch(t.test(hv, cm, paired = TRUE)$p.value, error = function(e) NA_real_)) }))
for (cal in c(MATCH_CALIPER, Inf)) {
  p <- match_pairs(cal); sfx <- if (is.finite(cal)) "" else "_nocaliper"
  pt <- paired(p); pt[, `:=`(n_matched = uniqueN(p$host), median_match_dist = median(p$dist))]
  fwrite(p, out(sprintf("tab1d_match_assignments%s.csv", sfx))); fwrite(pt, out(sprintf("tab1d_matched_comparison%s.csv", sfx)))
  msg("matching (caliper %s): %d of %d hosting districts matched, median distance %.2f SD", cal, uniqueN(p$host), m0[hosting == 1, .N], median(p$dist))
  if (is.finite(cal)) msg("  unmatchable: %s", paste(setdiff(m0[hosting == 1, dist_name], unique(p$host_name)), collapse = ", "))
}

# ---- (v) regressions with state FE and Conley SEs -------------------------------------------
cen <- st_coordinates(st_transform(st_centroid(st_geometry(read_districts())), 4326))
cxy <- data.table(zone_uid = read_districts()$zone_uid, lon = cen[, 1], lat = cen[, 2])
r0 <- merge(fr, cxy, by = "zone_uid")
conley_se <- function(fit, lon, lat, km) {
  X <- model.matrix(fit)[, !is.na(coef(fit)), drop = FALSE]; e <- residuals(fit); w <- if (is.null(weights(fit))) rep(1, length(e)) else weights(fit)
  used <- as.integer(names(e)); lon <- lon[used]; lat <- lat[used]          # rows lm actually used
  Xe <- X * (e * w); rad <- pi / 180
  cosd <- outer(sin(lat * rad), sin(lat * rad)) + outer(cos(lat * rad), cos(lat * rad)) * cos(outer(lon * rad, lon * rad, "-"))
  cosd[cosd > 1] <- 1; dkm <- 6371 * acos(cosd)                           # (pmin() would drop the matrix dims)
  K <- (dkm <= km) * 1; meat <- t(Xe) %*% K %*% Xe; bread <- summary(fit)$cov.unscaled                          # (X'WX)^-1 via lm's QR (stable)
  sqrt(diag(bread %*% meat %*% bread))
}
CTRL <- "urban_share_2019 + ntl_mean + wealth_mean_2019 + share_scst_2019 + share_muslim_2019 + share_bpl_2019 + factor(st)"
reg <- rbindlist(lapply(intersect(c("pm25_acag_local", "no2_surf", "o3_summer", "o3_annual", "hot_days_peryr_era5"), names(r0)), function(y) {
  d <- r0[complete.cases(r0[, c(y, "hosting", "urban_share_2019", "ntl_mean", "wealth_mean_2019", "share_scst_2019", "share_muslim_2019", "share_bpl_2019"), with = FALSE])]
  fit <- lm(as.formula(paste(y, "~ hosting +", CTRL)), data = d)
  cs <- conley_se(fit, d$lon, d$lat, CONLEY_KM)
  out1 <- data.table(outcome = y, weighting = "none", n = nrow(d), coef_hosting = coef(fit)[["hosting"]],
                     se_ols = summary(fit)$coef["hosting", 2], se_conley_200km = cs[["hosting"]])
  if (y == "pm25_acag_local" && "pm25_acag_unc" %in% names(d)) {
    fw <- lm(as.formula(paste(y, "~ hosting +", CTRL)), data = d, weights = 1 / pm25_acag_unc^2)
    out1 <- rbind(out1, data.table(outcome = y, weighting = "1/unc^2", n = nrow(d), coef_hosting = coef(fw)[["hosting"]],
                                   se_ols = summary(fw)$coef["hosting", 2], se_conley_200km = conley_se(fw, d$lon, d$lat, CONLEY_KM)[["hosting"]]))
  }
  out1 }))
print(reg, digits = 3); fwrite(reg, out("tab3_siting_regressions.csv"))

# ---- P(hosting): logit / LPM with the full SES set and state FE ------------------------------
ph <- r0[complete.cases(r0[, .(hosting, urban_share_2019, ntl_mean, wealth_mean_2019, share_scst_2019, share_muslim_2019, share_bpl_2019)])]
lpm <- lm(as.formula(paste("hosting ~", CTRL)), data = ph)
lgt <- suppressWarnings(glm(hosting ~ urban_share_2019 + ntl_mean + wealth_mean_2019 + share_scst_2019 + share_muslim_2019 + share_bpl_2019,
                            data = ph, family = binomial()))
fwrite(rbind(data.table(model = "LPM + state FE", term = names(coef(lpm)), estimate = coef(lpm), se = summary(lpm)$coef[, 2], n = nrow(ph)),
             data.table(model = "logit (no FE)", term = names(coef(lgt)), estimate = coef(lgt), se = summary(lgt)$coef[, 2], n = nrow(ph)))[!grepl("factor\\(st\\)", term)],
       out("tab3b_phosting_models.csv"))

if (DC_SCOPE == "operating") {   # v2 goldens pinned 4 Oct 2026 (Census 2011 boundaries, site geocodes)
  assert(t1[variable == "Relative Wealth Index", abs(d_all_india - G(1.32, 1.337)) < 0.01], "RWI d should reproduce %.2f (Table S2.1)", G(1.32, 1.337))
  assert(t1[variable == "Surface NO2 (1 km annual)", abs(d_all_india - G(0.97, 0.814)) < 0.02], "surface NO2 d should reproduce %.2f", G(0.97, 0.814))
}
