# =============================================================================
# 13_burden.R  —  hosting-district health burden (A) and on-site water in
#                 water-stressed basins (B)
# -----------------------------------------------------------------------------
# A. GEMM (Burnett et al. 2018) NCD+LRI, adults 25+: attributable deaths from
#    ambient PM2.5 (ACAG 2013-2022 district mean) against the WHO 5 ug/m3
#    guideline counterfactual:
#       deaths = pop x ADULT_SHARE x BASE_RATE x (1 - HR(5)/HR(z))
#       HR(z)  = exp(theta * log(1 + z'/alpha) / (1 + exp(-(z' - mu)/nu))),  z' = max(z - 2.4, 0)
#    theta 0.143, alpha 1.6, mu 15.5, nu 36.8, TMREL 2.4 ug/m3.
#    ADULT_SHARE 0.562 (UN WPP 2022, India 2020, 25+); BASE_RATE 0.0087 (GBD
#    2023 Results; upper end of 0.0084-0.0087 for India 25+, 2019-2023). Subgroup burdens weight district deaths
#    by NFHS-5 group shares.
#    Reproduces (20 Oct 2026, v2 inventory): 61 districts, 243.6 M, 285,253 deaths;
#    all-India 1,707,861.
# B. Facility scope-1 (on-site) water x Aqueduct sub-basin class at the facility
#    point (05e). Current: 14.2 GL/yr in extremely-high-stress basins (3.9-23.6 across
#    WUE 0.5-3.0) = 63% of the 22.6 GL scope-1 total; about 45 GL at 6.5 GW by 2030.
# =============================================================================
source(file.path(Sys.getenv("DCEJ_ROOT", unset = (function(){a<-commandArgs(FALSE);f<-sub("^--file=","",grep("^--file=",a,value=TRUE));d<-if(length(f)) dirname(normalizePath(f[1])) else getwd();while(!file.exists(file.path(d,"scripts","00_config.R"))&&d!=dirname(d)) d<-dirname(d);d})()), "scripts", "00_config.R"))

ADULT_SHARE <- 0.562; BASE_RATE <- 0.0087; CF <- 5
gemm_hr <- function(z, theta = 0.143, alpha = 1.6, mu = 15.5, nu = 36.8, tmrel = 2.4) {
  z <- pmax(z - tmrel, 0); exp(theta * log(1 + z / alpha) / (1 + exp(-(z - mu) / nu)))
}

# ---- A. health ------------------------------------------------------------------------
fr <- fread(path_frame)
host <- if (DC_SCOPE == "operating") "hosting" else "hosting_all"
fr[, deaths := pop_2020 * ADULT_SHARE * BASE_RATE * (1 - gemm_hr(CF) / gemm_hr(pm25_acag_local))]
h <- fr[get(host) == 1]
assert(!anyNA(h$pop_2020) && !anyNA(h$pm25_acag_local), "hosting district with NA population or ACAG PM2.5: %s", paste(h[is.na(pop_2020) | is.na(pm25_acag_local), dist_name], collapse = ", "))
tabA <- data.table(scope = DC_SCOPE, districts = nrow(h), residents_million = h[, sum(pop_2020)] / 1e6,
                   deaths_hosting = h[, sum(deaths, na.rm = TRUE)],
                   # subgroup deaths: districts without NFHS-5 shares (v2: Puducherry) contribute to the total but not
                   # to the subgroups; counted in districts_no_nfhs (fix 3 Oct 2026; was NA)
                   deaths_sc = h[, sum(deaths * share_sc_2019, na.rm = TRUE)], deaths_st = h[, sum(deaths * share_st_2019, na.rm = TRUE)],
                   deaths_obc = h[, sum(deaths * share_obc_2019, na.rm = TRUE)], deaths_muslim = h[, sum(deaths * share_muslim_2019, na.rm = TRUE)],
                   deaths_bpl = h[, sum(deaths * share_bpl_2019, na.rm = TRUE)],
                   districts_no_nfhs = h[is.na(share_sc_2019), .N], deaths_no_nfhs = h[is.na(share_sc_2019), sum(deaths, na.rm = TRUE)], deaths_all_india = fr[, sum(deaths, na.rm = TRUE)],
                   adult_share = ADULT_SHARE, base_rate = BASE_RATE, counterfactual_ugm3 = CF)
print(tabA[, .(districts, residents_million = round(residents_million, 1), deaths_hosting = round(deaths_hosting),
               deaths_sc = round(deaths_sc), deaths_obc = round(deaths_obc), deaths_all_india = round(deaths_all_india))])
fwrite(tabA, out("tab13a_health_burden.csv"))
if (DC_SCOPE == "operating") assert(abs(tabA$deaths_hosting - G(240442, 285253)) < 1500, "hosting burden moved away from %.0f (got %.0f)", G(240442, 285253), tabA$deaths_hosting)   # v2 pinned 4 Oct 2026

# ---- B. water in stressed basins -------------------------------------------------------------
wf <- out("tab05e_water_facility.csv")
if (!file.exists(wf)) { msg("section B skipped: run 05e_water.R first"); quit(save = "no") }
w  <- fread(wf)
fx <- merge(fread(out("tab15_facilities.csv")), w[, .(dc_id, bws_raw, bws_ext_high, bws_high_plus)], by = "dc_id")
assert(nrow(fx) == nrow(fread(out("tab15_facilities.csv"))), "every facility needs a water-stress class")
gp <- fread(path_grid_params); wue <- unlist(gp[param == "wue_l_per_kwh_it", .(low, central, high)])
s1 <- fx[bws_ext_high == TRUE, sum(scope1_ml)] / 1000; s1_all <- fx[, sum(scope1_ml)] / 1000
tabB <- data.table(scope = DC_SCOPE, facilities = nrow(fx), n_ext_high = fx[bws_ext_high == TRUE, .N], n_high_plus = fx[bws_high_plus == TRUE, .N],
                   scope1_gl_total = s1_all, scope1_gl_ext_high = s1, share_ext_high = s1 / s1_all,
                   scope1_gl_ext_high_wue_low = s1 * wue[["low"]] / wue[["central"]], scope1_gl_ext_high_wue_high = s1 * wue[["high"]] / wue[["central"]],
                   scope1_gl_ext_high_2030 = s1 * gp[param == "growth_mw_2030", central] / fx[, sum(mw_it)])
print(tabB)
fwrite(fx[, .(dc_id, name, operator, dist_name, state_name, mw_it, scope1_ml, bws_raw, bws_ext_high, bws_high_plus)], out("tab13b_water_facility.csv"))
fwrite(tabB, out("tab13b_water_demand.csv"))
