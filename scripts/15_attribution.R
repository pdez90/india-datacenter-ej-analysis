# =============================================================================
# 15_attribution.R  —  facility IT capacity allocation and operational footprint
# -----------------------------------------------------------------------------
# Manuscript section 2.4, Equations 1-6; SI S4 (Table S4.1).
#
# Capacity. Reported IT loads are held fixed. The commercial subset (hyperscale
# + colocation) is calibrated to a national anchor A of operational third-party
# IT capacity:  lambda = (A - V_C) / W_C, where V_C is the reported load inside
# the commercial subset and W_C the summed class weights of commercial
# facilities with no reported load; each such facility gets w_i * lambda.
# Non-commercial facilities (telecom, enterprise) keep reported loads; the rest
# are sized at the same lambda WITHOUT rescaling to any total ("derived" MW).
# Class weights: hyperscale 10, colocation 3, enterprise 1, telecom 0.5; government 1 (v2; a
# government agency's own data centre, treated like an enterprise facility: non-commercial, not anchored)
# (manuscript ¶94/¶100). Identity-screen rows flagged no_derived_mw get 0.
#
# All scope (stock + pipeline): lambda is fixed on the OPERATING commercial
# subset (v1: using build-out reported loads; v2: using current reported loads, i.e. the
# same lambda as the operating scope), and every facility without a reported build-out
# load is sized at that lambda. This is a scenario, not a sourced
# national estimate (anchor_basis records this).
#
# Footprint per facility (central scenario):
#   e_fac [GWh]   = MW_IT x PUE x u x 8.76
#   co2 [kt]      = e_fac x CEA average grid factor (0.71 t/MWh)
#   scope1 [ML]   = MW_IT x u x 8.76 x WUE            (WUE per kWh of IT energy)
#   scope2 [ML]   = e_fac x grid water intensity (3.6614 L/kWh; 2.3012 without hydro)
# The authoritative marginal CO2 and air pollutants are in 15b (tab15b_marginal.csv);
# co2_mt_tiermedian here is a scalar cross-check only (was misnamed co2_mt_marginal).
# =============================================================================
source(file.path(Sys.getenv("DCEJ_ROOT", unset = (function(){a<-commandArgs(FALSE);f<-sub("^--file=","",grep("^--file=",a,value=TRUE));d<-if(length(f)) dirname(normalizePath(f[1])) else getwd();while(!file.exists(file.path(d,"scripts","00_config.R"))&&d!=dirname(d)) d<-dirname(d);d})()), "scripts", "00_config.R"))

CLASS_WEIGHTS <- c(hyperscale = 10, colocation = 3, enterprise = 1, telecom = 0.5, government = 1)   # government (v2): as enterprise, non-commercial
COMMERCIAL    <- c("hyperscale", "colocation")

gp  <- fread(path_grid_params)
par <- function(name, lvl = "central") { v <- gp[param == name][[lvl]]; assert(length(v) == 1, "param %s missing", name); as.numeric(v) }

inv_all <- fread(outp("dc_current_inventory.csv", "all"))          # 340 rows (v2), both scopes' inputs
inv     <- fread(out("dc_current_inventory.csv"))                  # this scope
assert(all(inv$dc_type %in% names(CLASS_WEIGHTS)), "unknown dc_type in inventory")

# reported load used in this scope: operating = current; all = build-out where given
rep_of <- function(x, scope) if (scope == "operating") x$mw_reported else fcoalesce(x$mw_reported_buildout, x$mw_reported)

allocate <- function(x, anchor, scope) {
  x <- copy(x)
  x[, class_weight := fifelse(no_derived_mw, 0, CLASS_WEIGHTS[dc_type])]
  x[, mw_rep := rep_of(x, scope)]
  x[, commercial := dc_type %in% COMMERCIAL]
  base <- x[is_operating == TRUE & commercial == TRUE]              # calibration set: operating commercial
  # v2, all scope: lambda comes from the operating calibration on CURRENT reported loads (today's MW per
  # weight unit) and is applied to every facility without a reported (build-out) load. v1 calibrated on
  # build-out loads, which became infeasible once published build-outs of live campuses exceeded the anchor.
  cal  <- if (scope == "all" && !is_v1) base$mw_reported else base$mw_rep
  V_C  <- sum(cal, na.rm = TRUE)
  W_C  <- sum(base$class_weight[is.na(cal)])
  lambda <- (anchor - V_C) / W_C
  assert(lambda > 0, "anchor %.0f MW is below the reported commercial load %.1f MW (infeasible)", anchor, V_C)
  x[, mw_it := fifelse(!is.na(mw_rep), mw_rep, class_weight * lambda)]
  x[, capacity_basis := fcase(!is.na(mw_rep), "reported",
                              class_weight == 0, "none (identity screen: no derived MW)",
                              commercial, "anchored", default = "derived (non-commercial)")]
  list(x = x, lambda = lambda, V_C = V_C, W_C = W_C)
}

footprint <- function(x, lvl) {
  pue <- par("pue", lvl); u <- par("utilization", lvl); wue <- par("wue_l_per_kwh_it", lvl)
  x[, `:=`(e_fac_gwh   = mw_it * pue * u * 8.76,
           co2_kt      = mw_it * pue * u * 8.76 * par("grid_co2_t_per_mwh"),
           scope1_ml   = mw_it * u * 8.76 * wue,
           scope2_ml   = mw_it * pue * u * 8.76 * par("grid_water_l_per_kwh"),
           scope2_nohydro_ml = mw_it * pue * u * 8.76 * par("grid_water_nohydro_l_per_kwh"))]
  x
}

scen <- rbindlist(lapply(c("low", "central", "high"), function(lvl) {
  A <- par("anchor_commercial_mw", lvl)
  a <- allocate(inv, A, DC_SCOPE); x <- footprint(a$x, lvl)
  e_twh <- x[, sum(e_fac_gwh)] / 1000
  data.table(scenario = lvl, anchor_mw = A, lambda_mw_per_weight = a$lambda,
             anchor_basis = if (DC_SCOPE == "operating") "sourced national commercial IT capacity" else "SCENARIO, not a sourced national estimate (lambda fixed on operating commercial subset, current loads)",
             mw_anchored = x[commercial == TRUE, sum(mw_it)], mw_derived = x[commercial == FALSE, sum(mw_it)],
             total_mw = x[, sum(mw_it)], share_commercial = x[commercial == TRUE, sum(mw_it)] / x[, sum(mw_it)],
             e_fac = e_twh, pct_national_generation = 100 * e_twh / par("national_generation_twh"),
             co2_mt = x[, sum(co2_kt)] / 1000, co2_mt_tiermedian = e_twh * par("mef_tiermedian_t_per_mwh"),
             scope1_gl = x[, sum(scope1_ml)] / 1000, scope2_gl = x[, sum(scope2_ml)] / 1000,
             scope2_gl_nohydro = x[, sum(scope2_nohydro_ml)] / 1000,
             coal_twh = e_twh * par("coal_share_generation"),
             coal_fleet_deaths = par("coal_fleet_deaths") * e_twh / par("national_generation_twh"),
             n_facilities = nrow(x), n_reported = x[capacity_basis == "reported", .N],
             mw_reported = x[capacity_basis == "reported", sum(mw_it)])
}))
print(scen[, .(scenario, anchor_mw, lambda = round(lambda_mw_per_weight, 4), total_mw = round(total_mw), e_twh = round(e_fac, 2),
               co2_mt = round(co2_mt, 1), s1_gl = round(scope1_gl, 1), s2_gl = round(scope2_gl, 1), deaths = round(coal_fleet_deaths))])
fwrite(scen, out("tab15_national.csv"))

# ---- facility table (central) ----------------------------------------------------------
a  <- allocate(inv, par("anchor_commercial_mw"), DC_SCOPE)
fx <- footprint(a$x, "central")
fx[, mw_per_weight_unit := a$lambda]
keep <- c("dc_id","name","operator","status","is_operating","src","dc_type","class_weight","commercial","no_derived_mw",
          "mw_reported","mw_reported_buildout","mw_rep","capacity_basis","mw_it","mw_per_weight_unit",
          "e_fac_gwh","co2_kt","scope1_ml","scope2_ml","scope2_nohydro_ml","zone_uid","dist_name","state_name","lon","lat")
fwrite(fx[, ..keep], out("tab15_facilities.csv"))
msg("central: lambda = %.4f MW per weight unit (%.1f MW per colocation facility); %d facilities; %.0f MW; %.2f TWh",
    a$lambda, 3 * a$lambda, nrow(fx), fx[, sum(mw_it)], fx[, sum(e_fac_gwh)] / 1000)

# ---- invariants -----------------------------------------------------------------------
cen <- scen[scenario == "central"]
assert(nrow(fx) == nrow(inv), "facility rows must equal inventory rows")
assert(all(c("capacity_basis", "class_weight", "mw_rep") %in% names(fx)), "provenance columns missing")
if (DC_SCOPE == "operating") {
  assert(abs(fx[commercial == TRUE, sum(mw_it)] - 1800) < 1e-6, "commercial MW must sum to the 1,800 MW anchor")
  assert(abs(cen$total_mw - G(2004.686, 2051.318)) < 0.01, "operating total must be %.1f MW, got %.3f", G(2004.686, 2051.318), cen$total_mw)
  assert(abs(cen$e_fac - G(19.668, 20.125)) < 0.001, "operating electricity must be %.3f TWh", G(19.668, 20.125))
}
if (DC_SCOPE == "all") assert(abs(cen$total_mw - G(5194.8, 8114.36)) < 0.5, "all-scope total must be ~%.1f MW, got %.1f", G(5194.8, 8114.36), cen$total_mw)

# ---- check against the frozen facility layer (v1 inventory only) --------------------------
if (is_v1) {
gf <- as.data.table(st_drop_geometry(st_read(file.path(dir_data, "golden", "facilities.geojson"), quiet = TRUE)))
sfx <- if (DC_SCOPE == "operating") "" else "_all"
gcols <- paste0(c("mw_it", "e_fac_gwh", "co2_kt", "scope1_ml", "scope2_ml"), sfx)
gf <- gf[, c("name", "operator", "dist_name", gcols), with = FALSE]; setnames(gf, gcols, paste0("g_", c("mw_it","e_fac_gwh","co2_kt","scope1_ml","scope2_ml")))
m <- merge(fx, gf, by = c("name", "operator", "dist_name"))
assert(nrow(m) == nrow(fx), "could not match every facility to the frozen layer (%d of %d)", nrow(m), nrow(fx))
dif <- m[, .(mw_it = max(abs(mw_it - g_mw_it), na.rm = TRUE), e_fac_gwh = max(abs(e_fac_gwh - g_e_fac_gwh), na.rm = TRUE),
             co2_kt = max(abs(co2_kt - g_co2_kt), na.rm = TRUE), scope1_ml = max(abs(scope1_ml - g_scope1_ml), na.rm = TRUE),
             scope2_ml = max(abs(scope2_ml - g_scope2_ml), na.rm = TRUE))]
print(dif); fwrite(dif, out("check15_vs_frozen.csv"))
assert(dif$mw_it < 1e-6 && dif$e_fac_gwh < 1e-3, "facility allocation no longer reproduces the frozen layer")
}
