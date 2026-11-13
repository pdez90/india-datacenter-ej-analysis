# =============================================================================
# 20b_inmap_disparity.R  —  population-weighted disparity in the InMAP PM2.5 increment (SI Table S4.1.1)
# -----------------------------------------------------------------------------
# Same estimator as 20a ("Sarkar-inspired population-weighted standardized disparity"; weights
# pop_2020 x group share; Z = difference of group means / pooled SD with Kish effective n), applied to
# the district increment dpm25 from 16d, UNTRANSFORMED (no log; the increment has many near-zero
# districts). Ratio = group mean / reference mean. Uncertainty: 2,000-draw bootstrap resampling whole
# states -> two-sided bootstrap sign probability (not a p-value; * when < 0.05). Wealth groups:
# population-weighted quintiles of districts (reference = richest), as in 20a.
# Regional table: population-weighted increment, p99 - p1 across districts, and rural/urban ratio by
# zone (North, Central, East, West, South, Northeast).
# Self-test: on the submitted paper's district increments (run 3) the point estimates must reproduce
# Table S4.1.1: NoElec 0.640 / -0.263, Rural 0.767 / -0.183, Muslim 0.692 / -0.225, Poorest 0.592 /
# -0.375, BPL 1.031 / +0.021 (ratio to 0.003, Z to 0.005).
# Run 3 also reported SC/ST 1.296 / +0.167 and OBC 1.421 / +0.220; this code gives 1.317 / +0.177 and
# 1.444 / +0.230 on the same increments (the caste shares of the v1 frame differ slightly from the
# one used on 1 Sep). Run 3's regional North (0.0202) is reproduced only if Jammu & Kashmir is left out
# of North; here it is included.
# Outputs: tab20b_inmap_disparity_national.csv, tab20b_inmap_regional.csv, tab20b_inmap_regional_disparity.csv
# =============================================================================
source(file.path(Sys.getenv("DCEJ_ROOT", unset = (function(){a<-commandArgs(FALSE);f<-sub("^--file=","",grep("^--file=",a,value=TRUE));d<-if(length(f)) dirname(normalizePath(f[1])) else getwd();while(!file.exists(file.path(d,"scripts","00_config.R"))&&d!=dirname(d)) d<-dirname(d);d})()), "scripts", "00_config.R"))
suppressMessages(library(sf))
B_BOOT <- as.integer(Sys.getenv("DCEJ_B_BOOT", "2000")); SEED <- 20260901

prep <- function(fr) {
  fr <- fr[!is.na(pop_2020) & pop_2020 > 0 & !is.na(wealth_mean_2019) & !is.na(dpm25)]
  fr[, `:=`(rural = 1 - urban_share_2019, nonbpl = 1 - share_bpl_2019, haselec = 1 - share_no_electricity_2019, st2015 = state_name_2015)]
  setorder(fr, wealth_mean_2019); fr[, cp := cumsum(pop_2020) / sum(pop_2020)]
  fr[, wq := pmin(5L, as.integer(ceiling(cp * 5 - 1e-9)))]
  for (q in 1:5) fr[, (paste0("wq", q)) := as.numeric(wq == q)]
  fr
}
contrasts <- data.table(
  label = c("SC/ST vs General", "OBC vs General", "BPL vs non-BPL", "No electricity vs has electricity", "Rural vs urban",
            "Muslim vs Hindu", "Poorest quintile vs richest", "Poorer quintile vs richest", "Middle quintile vs richest", "Richer quintile vs richest"),
  g   = c("share_scst_2019", "share_obc_2019", "share_bpl_2019", "share_no_electricity_2019", "rural", "share_muslim_2019", "wq1", "wq2", "wq3", "wq4"),
  ref = c("share_general_2019", "share_general_2019", "nonbpl", "haselec", "urban_share_2019", "share_hindu_2019", "wq5", "wq5", "wq5", "wq5"))
wmom <- function(x, w) { k <- !is.na(x) & w > 0; x <- x[k]; w <- w[k]; W <- sum(w); m <- sum(w * x) / W
  c(m = m, v = sum(w * (x - m)^2) / W, n = W^2 / sum(w^2)) }
zstat <- function(x, wg, wr) { a <- wmom(x, wg); b <- wmom(x, wr)
  s <- sqrt(((a[["n"]] - 1) * a[["v"]] + (b[["n"]] - 1) * b[["v"]]) / (a[["n"]] + b[["n"]] - 2))
  if (!is.finite(s) || s == 0) return(NA_real_); (a[["m"]] - b[["m"]]) / s }
pwc <- function(x, w) { k <- !is.na(x) & w > 0; sum(w[k] * x[k]) / sum(w[k]) }
point <- function(fr) rbindlist(lapply(seq_len(nrow(contrasts)), function(j) { cc <- contrasts[j]
  wg <- fr$pop_2020 * fr[[cc$g]]; wr <- fr$pop_2020 * fr[[cc$ref]]
  data.table(contrast = cc$label, pwc_group = pwc(fr$dpm25, wg), pwc_ref = pwc(fr$dpm25, wr), Z = zstat(fr$dpm25, wg, wr)) }))[, ratio := pwc_group / pwc_ref][]

# ---- self-test on run 3 ----------------------------------------------------------------------
gold_path <- file.path(dir_data, "golden", "districts.geojson")
if (DC_SCOPE == "operating" && file.exists(gold_path) && file.exists(file.path(dir_derived, "analysis_district_v1.csv"))) {
  g <- as.data.table(st_drop_geometry(st_read(gold_path, quiet = TRUE)))[, .(zone_uid, dpm25)]
  p1 <- point(prep(merge(fread(file.path(dir_derived, "analysis_district_v1.csv")), g, by = "zone_uid")))
  chk <- data.table(contrast = c("No electricity vs has electricity", "Rural vs urban", "Muslim vs Hindu", "Poorest quintile vs richest", "BPL vs non-BPL"),
                    r = c(0.640, 0.767, 0.692, 0.592, 1.031), z = c(-0.263, -0.183, -0.225, -0.375, 0.021))
  chk <- merge(chk, p1, by = "contrast")
  msg("self-test vs Table S4.1.1 (run 3): max |ratio diff| %.4f, max |Z diff| %.4f", chk[, max(abs(ratio - r))], chk[, max(abs(Z - z))])
  assert(chk[, max(abs(ratio - r))] < 0.003 && chk[, max(abs(Z - z))] < 0.005, "20b no longer reproduces Table S4.1.1")
}

# ---- this run ----------------------------------------------------------------------------------
# Sensitivity runs (10 Oct 2026, reviewer request): DCEJ_SENS=<name> keeps every output of this
# script apart from the baseline by suffixing file names with _<name> (see scripts/run_sensitivity.R).
SENS <- Sys.getenv("DCEJ_SENS", ""); SFX <- if (nzchar(SENS)) paste0("_", SENS) else ""
d16 <- fread(out(sprintf("tab16_inmap_district%s.csv", SFX)))[, .(zone_uid, dpm25)]
fr <- prep(merge(fread(path_frame), d16, by = "zone_uid"))
res <- point(fr)
set.seed(SEED); states <- unique(fr$st2015); idx <- split(seq_len(nrow(fr)), fr$st2015)
boot <- replicate(B_BOOT, { s <- sample(states, length(states), replace = TRUE); d <- fr[unlist(idx[s], use.names = FALSE)]
  vapply(seq_len(nrow(contrasts)), function(j) zstat(d$dpm25, d$pop_2020 * d[[contrasts$g[j]]], d$pop_2020 * d[[contrasts$ref[j]]]), 0) })
res[, boot_sign_prob := pmin(1, apply(boot, 1, function(z) { z <- z[is.finite(z)]
  2 * min((1 + sum(z <= 0)) / (length(z) + 1), (1 + sum(z >= 0)) / (length(z) + 1)) }))]
res[, `:=`(z_lo = apply(boot, 1, quantile, 0.025, na.rm = TRUE), z_hi = apply(boot, 1, quantile, 0.975, na.rm = TRUE))]
res[, star := fifelse(boot_sign_prob < 0.05, "*", "")]
print(res[, .(contrast, ratio = round(ratio, 3), Z = round(Z, 3), p = round(boot_sign_prob, 3), star)])
fwrite(cbind(scope = DC_SCOPE, inventory = INVENTORY, res), out(sprintf("tab20b_inmap_disparity_national%s.csv", SFX)))

# ---- regional ----------------------------------------------------------------------------------
ZONES <- list(
  North = c("Jammu & Kashmir", "Himachal Pradesh", "Punjab", "Chandigarh", "Haryana", "Nct Of Delhi", "Rajasthan", "Uttarakhaand"),
  Central = c("Uttar Pradesh", "Madhya Pradesh", "Chattisgarh"),
  East = c("Bihar", "Jharkhand", "Odisha", "West Bengal", "Andaman & Nicobar Islands"),
  West = c("Gujarat", "Maharashtra", "Goa", "Dadra & Nagar Haveli", "Daman & Diu"),
  South = c("Andhra Pradesh", "Telangana", "Karnataka", "Kerala", "Tamil Nadu", "Puducherry", "Lakshadweep"),
  Northeast = c("Assam", "Arunachal Pradesh", "Manipur", "Meghalaya", "Mizoram", "Nagaland", "Sikkim", "Tripura"))
zmap <- rbindlist(lapply(names(ZONES), function(z) data.table(state_name_2015 = ZONES[[z]], zone = z)))
fz <- merge(fr, zmap, by = "state_name_2015", all.x = TRUE)
if (fz[is.na(zone), .N]) msg("%d district(s) with no state name left out of the regional table (%.2f M people)", fz[is.na(zone), .N], fz[is.na(zone), sum(pop_2020)] / 1e6)
reg <- fz[!is.na(zone), .(districts = .N, population_million = sum(pop_2020) / 1e6, pw_increment = sum(pop_2020 * dpm25) / sum(pop_2020),
                          p99_minus_p1 = diff(quantile(dpm25, c(0.01, 0.99))),
                          rural_over_urban = pwc(dpm25, pop_2020 * rural) / pwc(dpm25, pop_2020 * urban_share_2019)), by = zone]
reg <- rbind(reg, fr[, .(zone = "India", districts = .N, population_million = sum(pop_2020) / 1e6, pw_increment = sum(pop_2020 * dpm25) / sum(pop_2020),
                         p99_minus_p1 = diff(quantile(dpm25, c(0.01, 0.99))), rural_over_urban = pwc(dpm25, pop_2020 * rural) / pwc(dpm25, pop_2020 * urban_share_2019))])
print(reg, digits = 3)
fwrite(cbind(scope = DC_SCOPE, inventory = INVENTORY, reg), out(sprintf("tab20b_inmap_regional%s.csv", SFX)))
# ---- regional disparity (effect sizes by zone; SI Table S4.1.2) --------------------------------
# Same estimator as the national table, applied within each zone to the six social contrasts (the
# wealth-quintile contrasts are defined on national population-weighted quintiles and degenerate
# within zones, e.g. the South holds no poorest-quintile district). The bootstrap resamples the
# zone's states, so zones with few states (Central: 3) give coarse sign probabilities. Z_national is
# the national Z of the same contrast; main-text 3.4 quotes Z / Z_national for North rural-vs-urban
# (about 4.4) and South SC/ST (about 2.5).
soc <- contrasts[1:6]
zsoc <- function(d) vapply(seq_len(nrow(soc)), function(j) zstat(d$dpm25, d$pop_2020 * d[[soc$g[j]]], d$pop_2020 * d[[soc$ref[j]]]), 0)
set.seed(SEED)
regd <- rbindlist(lapply(names(ZONES), function(z) { d <- fz[zone == z]
  r <- point(d)[contrast %in% soc$label][, zone := z]
  st <- unique(d$st2015); ix <- split(seq_len(nrow(d)), d$st2015)
  bz <- replicate(B_BOOT, { s <- sample(st, length(st), replace = TRUE); zsoc(d[unlist(ix[s], use.names = FALSE)]) })
  r[, boot_sign_prob := pmin(1, apply(bz, 1, function(x) { x <- x[is.finite(x)]; if (!length(x)) return(NA_real_)
    2 * min((1 + sum(x <= 0)) / (length(x) + 1), (1 + sum(x >= 0)) / (length(x) + 1)) }))]
  r[, `:=`(districts = nrow(d), states = uniqueN(d$st2015))]; r }))
regd <- merge(regd, res[, .(contrast, Z_national = Z, ratio_national = ratio)], by = "contrast")
regd[, zone := factor(zone, levels = names(ZONES))]; setorder(regd, zone, contrast)
setcolorder(regd, c("zone", "districts", "states", "contrast", "pwc_group", "pwc_ref", "ratio", "Z", "boot_sign_prob", "ratio_national", "Z_national"))
print(regd[, .(zone, contrast, ratio = round(ratio, 3), Z = round(Z, 3), p = round(boot_sign_prob, 3), Z_national = round(Z_national, 3))])
fwrite(cbind(scope = DC_SCOPE, inventory = INVENTORY, regd), out(sprintf("tab20b_inmap_regional_disparity%s.csv", SFX)))
msg("20b done")
