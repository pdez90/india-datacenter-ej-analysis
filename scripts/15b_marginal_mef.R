# =============================================================================
# 15b_marginal_mef.R  —  facility-level marginal CO2, SO2, NOx, primary PM2.5
# -----------------------------------------------------------------------------
# Each facility's electricity (tab15_facilities.csv, central scenario) times the
# flat-load marginal factor of its LOAD state (Sengupta et al. 2022; SI Table
# S4.3). v2: each facility takes its own state's factors, Telangana included
# (Sengupta et al. publish a Telangana row; decided 3 Oct 2026). v1 (the submitted
# paper) gave Telangana facilities the Andhra Pradesh factors; kept for the v1
# regression path. Factors: data/sengupta_mef_flatload.csv from 15a
# (full precision); otherwise the rounded SI Table S4.3 with a warning.
# Golden v1 (operating): 14.65 Mt CO2, 109.8 kt SO2, 58.66 kt NOx, 8.10 kt PM2.5.
# v2 (20 Oct 2026, current): 15.1 Mt, 113.39 / 59.88 / 8.48 kt; all scope 46.84 Mt.
# (v1 Table S4.3 fallback: 14.65 / 109.7 / 58.4 / 8.14; v1 all scope 38.2 Mt CO2.)
# THIS table (not tab15_national's tier-median scalar) is the authoritative
# marginal estimate.
# =============================================================================
source(file.path(Sys.getenv("DCEJ_ROOT", unset = (function(){a<-commandArgs(FALSE);f<-sub("^--file=","",grep("^--file=",a,value=TRUE));d<-if(length(f)) dirname(normalizePath(f[1])) else getwd();while(!file.exists(file.path(d,"scripts","00_config.R"))&&d!=dirname(d)) d<-dirname(d);d})()), "scripts", "00_config.R"))

# state-name key: the 2015 shapefile spells some states differently from Sengupta et al.
# (Chattisgarh, Uttarakhaand, Nct Of Delhi); fold them to one key (fix 3 Oct 2026: Uttarakhand
# had silently fallen back to the national median).
norm <- function(s) { s <- tolower(gsub("[^A-Za-z]", "", s)); s <- sub("^nctof", "", s); s <- sub("^chhattisgarh$", "chattisgarh", s)
                      s <- sub("^uttarakhaand$", "uttarakhand", s); s <- sub("^orissa$", "odisha", s); s <- sub("^uttaranchal$", "uttarakhand", s); s }
f_full <- file.path(dir_data, "sengupta_mef_flatload.csv")
if (file.exists(f_full)) {
  mef <- fread(f_full); setnames(mef, tolower(names(mef)))
  mef <- mef[, .(key = norm(state), co2 = co2, so2 = so2, nox = nox, pm25 = pm25)]
  basis <- "KiltHub full-precision flat-load factors (15a)"
} else {
  mef <- fread(file.path(dir_data, "sengupta_mef_flatload_tableS43.csv"))
  mef <- mef[, .(key = norm(state_2015), co2 = co2_kg_mwh, so2 = so2_kg_mwh, nox = nox_kg_mwh, pm25 = pm25_kg_mwh)]
  basis <- "SI Table S4.3 (rounded) - run 15a for full precision"
  warning("15b: using the rounded SI Table S4.3 factors; run 15a_mef_kilthub.R for full precision", call. = FALSE)
}
msg("marginal factors: %s", basis)

fx <- fread(out("tab15_facilities.csv"))
# load_state_2015 keeps its historical column name; under v2 it is the facility's own state
fx[, load_state_2015 := if (is_v1) fifelse(state_name == "Telangana", "Andhra Pradesh", state_name) else state_name]
fx[, key := norm(load_state_2015)]
x <- merge(fx, mef, by = "key", all.x = TRUE)
miss <- x[is.na(co2), unique(load_state_2015)]
imp_f <- out("tab15b_mef_imputed_states.csv"); if (file.exists(imp_f)) file.remove(imp_f)   # never leave a stale list behind
if (length(miss)) {                  # impute a missing state with the national facility-weighted median, logged
  med <- lapply(mef[, .(co2, so2, nox, pm25)], median)
  x[is.na(co2), `:=`(co2 = med$co2, so2 = med$so2, nox = med$nox, pm25 = med$pm25)]
  fwrite(data.table(state = miss), out("tab15b_mef_imputed_states.csv"))
  msg("imputed factors for %d state(s): %s", length(miss), paste(miss, collapse = ", "))
}
assert(x[load_state_2015 %in% miss, sum(e_fac_gwh)] <= 0.05 * x[, sum(e_fac_gwh)], "more than 5%% of electricity needs imputed factors")
# every state that has its own row in Sengupta et al. must have matched it (an imputation is only for states they omit)
assert(!any(norm(miss) %in% mef$key), "state(s) %s have a factor row but did not match it", paste(miss, collapse = ", "))
if (!is_v1) assert(!length(miss) || all(miss %in% c("Chandigarh", "Puducherry")), "unexpected states without factors: %s", paste(miss, collapse = ", "))

# units: e [GWh] x factor [kg/MWh] = (e x 1000 MWh) x kg/MWh = e x 1000 kg = e tonnes
x[, `:=`(co2_t = e_fac_gwh * co2, so2_t = e_fac_gwh * so2, nox_t = e_fac_gwh * nox, pm25_t = e_fac_gwh * pm25)]
x[, mef_basis := basis]
keep <- c("dc_id","name","operator","status","dc_type","zone_uid","dist_name","state_name","load_state_2015","mw_it","e_fac_gwh",
          "co2","so2","nox","pm25","co2_t","so2_t","nox_t","pm25_t","mef_basis")
setorder(x, dc_id)
fwrite(x[, ..keep], out("tab15b_marginal.csv"))

tot <- x[, .(co2_mt = sum(co2_t) / 1e6, so2_kt = sum(so2_t) / 1e3, nox_kt = sum(nox_t) / 1e3, pm25_kt = sum(pm25_t) / 1e3, e_twh = sum(e_fac_gwh) / 1e3)]
print(tot)
st <- x[, .(facilities = .N, e_gwh = sum(e_fac_gwh), co2_kg_mwh = co2[1], so2_kg_mwh = so2[1], nox_kg_mwh = nox[1], pm25_kg_mwh = pm25[1],
            so2_t = sum(so2_t), nox_t = sum(nox_t), pm25_t = sum(pm25_t)), by = load_state_2015][order(-e_gwh)]
fwrite(st, out("tab15b_s43_hosting_factors.csv"))
fwrite(tot, out("tab15b_marginal_totals.csv"))
if (DC_SCOPE == "operating") {   # v2 goldens pinned 4 Oct 2026
  assert(abs(tot$co2_mt - G(14.65, 15.033)) < 0.05, "marginal CO2 should be ~%.2f Mt, got %.3f", G(14.65, 15.033), tot$co2_mt)
  assert(abs(tot$so2_kt - G(109.8, 111.50)) < 0.5 && abs(tot$nox_kt - G(58.66, 59.96)) < 0.3 && abs(tot$pm25_kt - G(8.10, 8.42)) < 0.05, "marginal pollutants moved away from the expected %s kt", G("109.8 / 58.7 / 8.1", "111.5 / 60.0 / 8.4"))
}
if (DC_SCOPE == "all") assert(abs(tot$co2_mt - G(38.2, 60.29)) < 0.2, "all-scope marginal CO2 should be ~%.2f Mt", G(38.2, 60.29))
