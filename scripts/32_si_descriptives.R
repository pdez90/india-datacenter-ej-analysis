# =============================================================================
# 32_si_descriptives.R  —  SI numbers that are derived from pipeline outputs rather than
# produced by a modelling step (operating inventory):
#   A. SI S4: capacity-weighted profile of the coal-generating districts vs the facility-weighted
#      hosting districts (133 districts, 409 M people; Singrauli ... Korba; wealth 84,400 = 53rd pct).
#   B. SI S4: electricity-related water at the thermal marginal mix (92% coal, 8% gas; coal 3.0 and
#      gas 0.92 L/kWh, Table S4.2) instead of the average grid (74.0 -> 57.2 GL/yr).
#   C. SI S4: unweighted mean of the ten season-period Sengupta factors vs the flat-load factors
#      (+1.6-2.6% attributable emissions).
#   D. SI S1.1.5: the four flagged operating loads (IN070, IN075, IN102, IN103) treated as modelled
#      (si_text = the value as printed in the SI; updated 30 Oct 2026 to the batch-9 inventory: -6.6 MW, about 0.6%).
#   E. SI S11 / main 3.4: share of attributable deaths in the four use states (52%).
#   G. SI S9: urban share of hosting districts in policy vs non-policy states (57% vs 70%; 70% each facility-weighted).
#   F. SI S12: district population, GHSL 2020 vs WorldPop 2020 and vs the GPW v4.11 population on
#      the InMAP grid (r = 0.99).
# Output: tab32_si_descriptives.csv (one row per number, with the value quoted in the SI)
# =============================================================================
source(file.path(Sys.getenv("DCEJ_ROOT", unset = (function(){a<-commandArgs(FALSE);f<-sub("^--file=","",grep("^--file=",a,value=TRUE));d<-if(length(f)) dirname(normalizePath(f[1])) else getwd();while(!file.exists(file.path(d,"scripts","00_config.R"))&&d!=dirname(d)) d<-dirname(d);d})()), "scripts", "00_config.R"))
if (is_v1 || DC_SCOPE != "operating") { msg("32: operating scope, v2 inventory only; skipped"); quit(save = "no") }

R <- list(); add <- function(item, value, si) R[[length(R) + 1]] <<- data.table(item = item, value = value, si_text = si)
fr  <- fread(path_frame)
wm  <- function(x, w) { k <- !is.na(x) & !is.na(w); sum(x[k] * w[k]) / sum(w[k]) }

# ---- A. coal-generating districts --------------------------------------------------------
cd <- fr[coal_mw > 0]; hd <- fr[dc_count > 0]
add("coal districts (any coal capacity)", nrow(cd), "133")
add("population of coal districts (million)", sum(cd$pop_2020, na.rm = TRUE) / 1e6, "414")
top <- cd[order(-coal_mw)][1:5]
for (i in 1:5) add(sprintf("coal capacity rank %d: %s (GW)", i, top$dist_name[i]), top$coal_mw[i] / 1000, c("11.2", "9.9", "9.2", "7.5", "6.4")[i])
prof <- list(wealth_mean_2019 = c("83,500", "882,200"), rwi_mean = c("-0.12", "+0.41"), urban_share_2019 = c("30%", "70%"),
             share_scst_2019 = c("36%", "25%"), share_bpl_2019 = c("52%", "31%"))
for (v in names(prof)) {
  add(sprintf("%s, coal districts (capacity-weighted)", v), wm(cd[[v]], cd$coal_mw), prof[[v]][1])
  add(sprintf("%s, hosting districts (facility-weighted)", v), wm(hd[[v]], hd$dc_count), prof[[v]][2])
}
add("percentile of the coal-district wealth mean among districts", 100 * mean(fr$wealth_mean_2019 <= wm(cd$wealth_mean_2019, cd$coal_mw), na.rm = TRUE), "53rd")

# ---- B. thermal-mix water -------------------------------------------------------------------
nat <- fread(out("tab15_national.csv")); e_twh <- sum(fread(out("tab15_facilities.csv"))$e_fac_gwh) / 1e3
thermal_l_kwh <- 0.92 * 3.0 + 0.08 * 0.92
gp <- fread(file.path(dir_data, "india_grid_params.csv"))
add("scope 2 water at the average grid (GL/yr)", e_twh * gp[param == "grid_water_l_per_kwh", as.numeric(central)], "74.0")
add("scope 2 water at the thermal marginal mix (GL/yr)", e_twh * thermal_l_kwh, "57.2")

# ---- C. unweighted vs flat-load marginal factors --------------------------------------------
long <- fread(file.path(dir_data, "sengupta_mef_long.csv"))
uw <- dcast(long[, .(v = mean(value)), by = .(state, pollutant)], state ~ pollutant, value.var = "v")
nk <- function(s) gsub("[^a-z]", "", tolower(s))
mx <- fread(out("tab15b_marginal.csv")); mx[, key := nk(load_state_2015)]; uw[, key := nk(state)]
m <- merge(mx, uw[, !"state"], by = "key", all.x = TRUE)
for (p in c("co2", "so2", "nox", "pm25")) {
  u <- m[[toupper(p)]]; u[is.na(u)] <- m[[p]][is.na(u)]       # states without a row keep their imputed factor
  add(sprintf("unweighted-mean factors: change in attributable %s (%%)", toupper(p)), 100 * (sum(m$e_fac_gwh * u) / sum(m$e_fac_gwh * m[[p]]) - 1),
      c(co2 = "1.9", so2 = "2.6", nox = "1.6", pm25 = "2.0")[[p]])
}

# ---- D. the four flagged loads treated as modelled ------------------------------------------
fx <- fread(out("tab15_facilities.csv"))
xw <- fread(out("dc_current_inventory.csv"))[, .(dc_id, facility_id)]
four <- xw[facility_id %in% c("IN070", "IN075", "IN102", "IN103"), dc_id]
assert(length(four) == 4, "32: the four flagged facilities were not all found")
assert(all(four %in% fx$dc_id) && fx[dc_id %in% four, all(commercial)], "32: the four flagged facilities must be operating commercial rows")
anchor <- 1800
com <- fx[commercial == TRUE & !no_derived_mw]; rep0 <- !is.na(com$mw_reported)
lam0 <- (anchor - sum(com$mw_reported[rep0])) / sum(com$class_weight[!rep0])
rep1 <- rep0 & !(com$dc_id %in% four)
lam1 <- (anchor - sum(com$mw_reported[rep1])) / sum(com$class_weight[!rep1])
fx[, mw_alt := mw_it]
fx[commercial == TRUE & !no_derived_mw & (is.na(mw_reported) | dc_id %in% four), mw_alt := class_weight * lam1]
fx[commercial == FALSE & !no_derived_mw & is.na(mw_reported), mw_alt := class_weight * lam1]
assert(abs(fx[commercial == FALSE & !no_derived_mw & is.na(mw_reported), sum(class_weight)] * lam0 + fx[commercial == FALSE & !is.na(mw_reported), sum(mw_reported)] -
           fx[commercial == FALSE, sum(mw_it)]) < 0.5, "32: could not reproduce the non-commercial allocation")
add("operating total MW, four flagged loads modelled minus baseline", sum(fx$mw_alt) - sum(fx$mw_it), "6.6 MW (in size)")
s <- merge(fx[, .(dc_id, mw_it, mw_alt)], mx[, .(dc_id, so2_t)], by = "dc_id")
add("attributable SO2, four flagged loads modelled (% change)", 100 * (s[, sum(so2_t * mw_alt / mw_it, na.rm = TRUE)] / s[, sum(so2_t)] - 1), "about 0.6%")

# ---- E. deaths in the four use states -----------------------------------------------------------
t16 <- fread(out("tab16_inmap_district.csv"))[weighting == "population"]
add("share of attributable deaths in MH, TN, KA, Delhi (%)", 100 * t16[state_name %in% c("Maharashtra", "Tamil Nadu", "Karnataka", "Nct Of Delhi", "Delhi"), sum(deaths)] / t16[, sum(deaths)], "52%")

# ---- G. SI S9: urban share of hosting districts, policy vs non-policy states -----------------------
pol <- fread(file.path(dir_data, "state_dc_policies.csv"))
ns  <- function(x) gsub("nctof|nct|\\(|\\)", "", gsub(" ", "", gsub("chattisgarh", "chhattisgarh", tolower(x))))
pol[, `:=`(key = ns(state), has = !startsWith(tolower(policy_name), "none"))]
hd[, key := ns(state_name)]; hp <- merge(hd, pol[, .(key, has)], by = "key")
# The merge legitimately drops hosting districts in states the policy file does not list at all
# (no dedicated policy and no "none" row). Check that nothing is lost ON TOP of those, and tie the
# policy-state facility count to 14's own table rather than to a pinned number, so the two scripts
# cannot silently disagree when the inventory changes. (Was a hard-coded 211/25; 211 went stale
# when the inventory was revised, 22 Oct 2026.)
lost <- hd[!key %in% pol$key]
assert(hp[, sum(dc_count)] + lost[, sum(dc_count)] == hd[, sum(dc_count)],
       "32: policy-state matching lost hosting districts (matched %g + unlisted %g != %g hosting facilities)",
       hp[, sum(dc_count)], lost[, sum(dc_count)], hd[, sum(dc_count)])
p14 <- fread(out("tab14_policy_burden.csv"))
assert(hp[has == TRUE, sum(dc_count)] == p14[has_policy == TRUE, sum(dc_count)] &&
       hp[has == FALSE, sum(dc_count)] == p14[has_policy == FALSE, sum(dc_count)],
       "32: policy-state split (%g with / %g without) disagrees with 14 (%g / %g)",
       hp[has == TRUE, sum(dc_count)], hp[has == FALSE, sum(dc_count)],
       p14[has_policy == TRUE, sum(dc_count)], p14[has_policy == FALSE, sum(dc_count)])
msg("32: %g facilities in dedicated-policy states, %g in listed states without one, %g in states with no policy row (%s)",
    hp[has == TRUE, sum(dc_count)], hp[has == FALSE, sum(dc_count)], lost[, sum(dc_count)],
    paste(sort(unique(lost$state_name)), collapse = ", "))
for (h in c(TRUE, FALSE)) {
  x <- hp[has == h]
  add(sprintf("urban share of hosting districts, %s states (unweighted, %%)", if (h) "policy" else "non-policy"), 100 * mean(x$urban_share_2019), if (h) "57%" else "70%")
  add(sprintf("urban share of hosting districts, %s states (facility-weighted, %%)", if (h) "policy" else "non-policy"), 100 * wm(x$urban_share_2019, x$dc_count), "70%")
}

# ---- F. population layers -------------------------------------------------------------------
wp <- fread(file.path(dir_gee, "gee_worldpop.csv")); gh <- fread(file.path(dir_gee, "gee_ghsl_2020.csv"))
pg <- merge(merge(wp, gh, by = "zone_uid"), t16[, .(zone_uid, pop_grid)], by = "zone_uid")
add("district population r, GHSL 2020 vs WorldPop 2020", cor(pg$ghsl_pop_2020, pg$worldpop_2020), "0.99")
add("district population r, GHSL 2020 vs GPW v4.11 (CIESIN 2018) on the InMAP grid", cor(pg$ghsl_pop_2020, pg$pop_grid), "0.99")

res <- rbindlist(R); res[, value := signif(value, 5)]
fwrite(res, out("tab32_si_descriptives.csv")); print(res)
