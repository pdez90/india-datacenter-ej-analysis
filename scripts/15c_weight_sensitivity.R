# =============================================================================
# 15c_weight_sensitivity.R  —  sensitivity to the operator-class capacity weights
# -----------------------------------------------------------------------------
# SI section S4 ("Sensitivity to the operator-class capacity weights").
# Re-runs the 15 allocation under five alternative weightings (hyperscale :
# colocation : enterprise : telecom) and recomputes each state's share of
# attributable marginal SO2, NOx and primary PM2.5 (load state, 2015 geography,
# Hyderabad in Andhra Pradesh), pollutants kept separate. The commercial total is
# fixed at the anchor by construction; the derived non-commercial component is not.
#   (hyperscale : colocation : enterprise : telecom, as in SI S4)
#   baseline 10:3:1:0.5 | flat 1:1:1:1 | compressed 5:2:1:0.5 | stretched 20:4:1:0.25
#   colo_led 6:6:1:0.5  | hyper_only 30:3:1:0.5
# v1 inventory values (30 Sep 2026; current v2 results: SI Table S4.1.3 and tab15c_*): max pp flat 7.81/7.61/8.30,
# compressed 2.72/2.64/2.88, stretched 4.10/3.98/4.36, colo_led 6.59/6.65/7.22,
# hyper_only 10.86/10.63/11.64 (SO2/NOx/PM2.5).
# Golden (v1): largest single state-share change 11.6 pp (hyper_only, PM2.5); mean
# absolute change <= 1.4 pp; AP SO2 16.8 -> 27.7% under hyper_only; four AI-use
# states (MH, TN, KA, DL) hold 68.1% of SO2 at baseline (61.6-72.4% across
# schemes); derived non-commercial MW 121.6 (stretched) .. 936.2 (flat) vs 204.7.
# BUG GUARD (from the original): the weight argument is `wsch`, never `w`
# (a data.table column `w` once shadowed it and every facility got weight 1).
# =============================================================================
source(file.path(Sys.getenv("DCEJ_ROOT", unset = (function(){a<-commandArgs(FALSE);f<-sub("^--file=","",grep("^--file=",a,value=TRUE));d<-if(length(f)) dirname(normalizePath(f[1])) else getwd();while(!file.exists(file.path(d,"scripts","00_config.R"))&&d!=dirname(d)) d<-dirname(d);d})()), "scripts", "00_config.R"))

#                   hyperscale colocation telecom enterprise government   (manuscript: enterprise 1, telecom 0.5 at baseline)
WEIGHTS <- list(baseline   = c(10, 3, 0.5, 1, 1),
                flat       = c(1, 1, 1, 1, 1),
                compressed = c(5, 2, 0.5, 1, 1),
                stretched  = c(20, 4, 0.25, 1, 1),
                colo_led   = c(6, 6, 0.5, 1, 1),
                hyper_only = c(30, 3, 0.5, 1, 1))
TYPES <- c("hyperscale", "colocation", "telecom", "enterprise", "government")   # government (v2) always weighted as enterprise
AI_STATES <- c("Maharashtra", "Tamil Nadu", "Karnataka", "Nct Of Delhi")

gp <- fread(path_grid_params); A <- gp[param == "anchor_commercial_mw", central]
e_per_mw <- gp[param == "pue", central] * gp[param == "utilization", central] * 8.76
mb <- fread(out("tab15b_marginal.csv"))                    # facility factors (kg/MWh) from 15b
inv <- fread(out("dc_current_inventory.csv"))
x0 <- merge(inv, mb[, .(dc_id, load_state_2015, co2, so2, nox, pm25)], by = "dc_id")
assert(nrow(x0) == nrow(inv), "15b facility factors missing for some facilities")
rep_col <- if (DC_SCOPE == "operating") x0$mw_reported else fcoalesce(x0$mw_reported_buildout, x0$mw_reported)

run_scheme <- function(wsch) {
  x <- copy(x0); x[, mw_rep := rep_col]
  x[, cw := fifelse(no_derived_mw, 0, wsch[match(dc_type, TYPES)])]
  assert(!anyNA(x$cw), "weight lookup returned NA")
  com <- x$dc_type %in% c("hyperscale", "colocation")
  base <- x[is_operating == TRUE & com]
  cal <- if (DC_SCOPE == "all" && !is_v1) base$mw_reported else base$mw_rep   # v2 all scope: today's lambda (as in 15)
  lam <- (A - sum(cal, na.rm = TRUE)) / sum(base$cw[is.na(cal)])
  x[, mw_it := fifelse(!is.na(mw_rep), mw_rep, cw * lam)]
  x[, e := mw_it * e_per_mw]
  st <- x[, .(so2 = sum(e * so2), nox = sum(e * nox), pm25 = sum(e * pm25)), by = load_state_2015]
  st[, `:=`(so2_share = so2 / sum(so2), nox_share = nox / sum(nox), pm25_share = pm25 / sum(pm25))]
  list(states = st, lambda = lam, derived_mw = x[!com, sum(mw_it)], total_mw = x[, sum(mw_it)])
}
runs <- lapply(WEIGHTS, run_scheme)
b <- runs$baseline$states
w1 <- unique(vapply(WEIGHTS, function(z) paste(z, collapse = ":"), ""))
assert(length(w1) == length(WEIGHTS), "two schemes have identical weights")

sens <- rbindlist(lapply(names(runs), function(s) {
  m <- merge(runs[[s]]$states, b[, .(load_state_2015, b_so2 = so2_share, b_nox = nox_share, b_pm = pm25_share)], by = "load_state_2015")
  m[, .(scheme = s, state = load_state_2015, so2_share, nox_share, pm25_share,
        d_so2_pp = 100 * (so2_share - b_so2), d_nox_pp = 100 * (nox_share - b_nox), d_pm25_pp = 100 * (pm25_share - b_pm))] }))
fwrite(sens, out("tab15c_weight_sensitivity.csv"))
# absolute load-state tonnes under each scheme, for the dispersion sensitivity (16b with DCEJ_SENS=w_<scheme>)
fwrite(rbindlist(lapply(names(runs), function(s) runs[[s]]$states[, .(scheme = s, load_state_2015, so2_t = so2, nox_t = nox, pm25_t = pm25)])),
       out("tab15c_weight_state_tonnes.csv"))
summ <- sens[, .(max_so2_pp = max(abs(d_so2_pp)), max_nox_pp = max(abs(d_nox_pp)), max_pm25_pp = max(abs(d_pm25_pp)),
                 mean_abs_pp = mean(abs(c(d_so2_pp, d_nox_pp, d_pm25_pp))),
                 ai4_so2_share = 100 * sum(so2_share[state %in% AI_STATES])), by = scheme]
summ[, `:=`(derived_noncommercial_mw = vapply(scheme, function(s) runs[[s]]$derived_mw, 0),
            total_mw = vapply(scheme, function(s) runs[[s]]$total_mw, 0))]
print(summ, digits = 3)
fwrite(summ, out("tab15c_weight_summary.csv"))
ap <- sens[state == "Andhra Pradesh" & scheme %in% c("baseline", "hyper_only"), .(scheme, so2 = 100 * so2_share, nox = 100 * nox_share, pm25 = 100 * pm25_share)]
print(ap, digits = 3)
if (DC_SCOPE == "operating") {
  assert(abs(summ[scheme == "baseline", derived_noncommercial_mw] - G(204.7, 251.32)) < 0.5, "baseline derived MW should be %.1f", G(204.7, 251.32))
  msg("largest single change: %.1f pp (submitted paper: 11.6); four-state SO2 share %.1f%% (submitted paper: 68.1)",
      summ[, max(max_so2_pp, max_nox_pp, max_pm25_pp)], summ[scheme == "baseline", ai4_so2_share])
}
