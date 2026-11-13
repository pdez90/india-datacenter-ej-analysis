# =============================================================================
# check_pipeline.R  —  end-of-run checks and a snapshot of every headline number
# -----------------------------------------------------------------------------
# Rebuilt 3 Oct 2026. Run last (run_all.R does). It
#   1. checks that every output the paper uses exists, for both scopes, and is not older than its inputs
#      (a fixed list of producer -> product pairs; a product older than its input is STALE);
#   2. checks cross-file invariants (counts, totals, conservation of emissions, scope/inventory stamps);
#   3. writes outputs/run_logs/headline_snapshot_<date>.csv: one row per headline number with the file it
#      came from, and compares it with the most recent earlier snapshot, listing every number that moved
#      (this is the reproducibility check: a rerun on unchanged code and data must move nothing beyond
#      InMAP's convergence tolerance);
#   4. writes outputs/run_logs/output_manifest_<date>.csv (md5 of every output table) for archiving.
# Exit status 1 if any check FAILS or any product is MISSING/STALE.
# =============================================================================
source(file.path(Sys.getenv("DCEJ_ROOT", unset = (function(){a<-commandArgs(FALSE);f<-sub("^--file=","",grep("^--file=",a,value=TRUE));d<-if(length(f)) dirname(normalizePath(f[1])) else getwd();while(!file.exists(file.path(d,"scripts","00_config.R"))&&d!=dirname(d)) d<-dirname(d);d})()), "scripts", "00_config.R"))
res <- list(); add <- function(check, status, detail = "") res[[length(res) + 1]] <<- data.table(check = check, status = status, detail = detail)
chk <- function(check, ok, detail = "") add(check, if (isTRUE(ok)) "PASS" else "FAIL", detail)
O <- function(f) outp(f, "operating"); A <- function(f) outp(f, "all")

# ---- 1. products and freshness ---------------------------------------------------------------
pairs <- list(
  c("dc_current_inventory.csv", "tab15_facilities.csv"), c("tab15_facilities.csv", "tab15b_marginal.csv"),
  c("tab15b_marginal.csv", "tab16b_emissions_points.csv"), c("tab16_inmap_district.csv", "tab16e_group_exposure.csv"),
  c("tab16_inmap_district.csv", "tab20b_inmap_disparity_national.csv"), c("tab16_inmap_district.csv", "tab21_benefit_burden.csv"),
  c("tab15b_marginal_totals.csv", "tab29_diesel_scenarios.csv"), c("tab16_inmap_district.csv", "tab20b_inmap_regional_disparity.csv"))
# operating-scope only products (30's public tables are built from the operating run)
pairs_op <- list(c("tab28_esmi_district_outages.csv", "tab29_diesel_district.csv"), c("tab29_diesel_district.csv", "public/power_interruptions_backup_diesel.csv"),
                 c("tab15b_s43_hosting_factors.csv", "public/emission_factors.csv"))
for (sc in c("operating", "all")) for (p in c(pairs, if (sc == "operating") pairs_op)) {
  a <- outp(p[1], sc); b <- outp(p[2], sc)
  if (!file.exists(b)) add(sprintf("[%s] %s", sc, p[2]), "MISSING", "") else
    add(sprintf("[%s] %s newer than %s", sc, p[2], p[1]), if (!file.exists(a) || file.mtime(b) >= file.mtime(a)) "PASS" else "STALE")
}
for (f in c("tab1_dc_vs_nodc_profile.csv", "tab1c_within_state_urban_profile.csv", "tab1d_matched_comparison.csv", "tab10b_permutation.csv",
            "tab3_siting_regressions.csv", "tab7c_dc_vs_coal.csv", "tab13a_health_burden.csv", "tab13b_water_demand.csv", "tab14_policy_burden.csv",
            "tab15_national.csv", "tab15c_weight_summary.csv", "tab16_inmap_summary.txt", "tab20a_disparity_national.csv",
            "tab20a_group_exposure_S61.csv", "tab23_monetised_summary.txt", "tab28_esmi_district_outages.csv", "tab29_diesel_summary.txt",
            "tab17b_intensity.csv", "tab17c_metro_fe.csv", "tab17d_ixp.csv", "tab17e_confidence_sensitivity.csv", "public/india_datacentres_public.csv",
            "tab3b_phosting_models.csv", "tab20b_inmap_regional.csv", "tab20b_inmap_regional_disparity.csv", "tab16_receiving_profile.csv",
            "tab13b_water_facility.csv", "tab15c_weight_sensitivity.csv", "tab16b_emissions_by_state.csv", "tab16b_source_split.csv",
            "tab23_monetised_ledger.csv", "tab23_ledger_parameters.csv", "tab14b_policy_provenance.csv", "tab19_geocode_provenance.csv",
            "tab7c_grid_shadow.csv", "tab1d_matched_comparison_nocaliper.csv", "tab29_diesel_district.csv", "tab28_esmi_hourofday.csv",
            "public/power_interruptions_backup_diesel.csv", "public/power_interruptions_backup_diesel_README.txt",
            "public/emission_factors.csv", "public/emission_factors_README.txt"))
  add(sprintf("[operating] %s", f), if (file.exists(O(f))) "PASS" else "MISSING")

# ---- 2. invariants ---------------------------------------------------------------------------------
io <- fread(O("dc_current_inventory.csv")); ia <- fread(A("dc_current_inventory.csv"))
chk("inventory sizes", nrow(io) == G(209, V2_N_OP) && nrow(ia) == G(297, V2_N_ALL), sprintf("%d / %d", nrow(io), nrow(ia)))
chk("operating facilities are a subset of the all scope", all(io$facility_id %in% ia$facility_id))
for (sc in c("operating", "all")) {
  fx <- fread(outp("tab15_facilities.csv", sc)); nat <- fread(outp("tab15_national.csv", sc))[scenario == "central"]
  chk(sprintf("[%s] facility MW sum = national total", sc), abs(sum(fx$mw_it) - nat$total_mw) < 1e-6)
  mb <- fread(outp("tab15b_marginal_totals.csv", sc)); ep <- fread(outp("tab16b_emissions_points.csv", sc))
  chk(sprintf("[%s] 16b conserves 15b SO2/NOx/PM2.5", sc), abs(sum(ep$so2_t) / 1e3 - mb$so2_kt) < 1e-6 * mb$so2_kt &&
        abs(sum(ep$nox_t) / 1e3 - mb$nox_kt) < 1e-6 * mb$nox_kt && abs(sum(ep$pm25_t) / 1e3 - mb$pm25_kt) < 1e-6 * mb$pm25_kt)
  chk(sprintf("[%s] no imputed emission factors", sc), !file.exists(outp("tab15b_mef_imputed_states.csv", sc)))
  s <- readLines(outp("tab16_inmap_summary.txt", sc))
  chk(sprintf("[%s] InMAP summary matches scope and inventory", sc), any(grepl(sprintf("DC_SCOPE=%s, INVENTORY=%s", sc, INVENTORY), s, fixed = TRUE)))
  chk(sprintf("[%s] InMAP field converged", sc), !any(grepl("NOT CONVERGED", s)))
  d16 <- fread(outp("tab16_inmap_district.csv", sc)); chk(sprintf("[%s] %d districts with a non-negative increment", sc, N_DIST), nrow(d16) == N_DIST && all(d16$dpm25 >= 0, na.rm = TRUE) && !anyNA(d16$dpm25))
}
h <- fread(O("tab13a_health_burden.csv")); chk("health-burden subgroups are numbers", !anyNA(unlist(h[, .(deaths_sc, deaths_st, deaths_obc, deaths_muslim, deaths_bpl)])))
pub <- fread(O("public/india_datacentres_public.csv"))
chk("public table: one row per facility, every row sourced", nrow(pub) == nrow(ia) && all(grepl("^https?://", pub$source_url)))
chk("public MW used in paper = operating MW", abs(sum(pub$mw_it_used_in_paper, na.rm = TRUE) - sum(fread(O("tab15_facilities.csv"))$mw_it)) < 0.5)
chk("DCEJ_SOFT_ASSERTS is not set", Sys.getenv("DCEJ_SOFT_ASSERTS") != "1", "soft asserts would have downgraded every golden to a message")
# ---- 2b. v2 goldens for the InMAP-derived and later numbers (pinned 4 Oct 2026; v1 runs are exempt) ------------
if (INVENTORY == "v2") {
  near <- function(x, g, tol) is.finite(x) && abs(x - g) <= tol * abs(g)
  # 26 Oct 2026: PROVISIONAL_PINS = TRUE after the batch-9 (capacity method) rerun was started and before its outputs were
  # pinned: each golden below is reported, not enforced. Set FALSE and update the numbers once the rerun has been reviewed.
  PROVISIONAL_PINS <- FALSE   # 29 Oct 2026: pinned from the batch-9 rerun (baseline 26 Oct, sensitivities 29 Oct)
  chk_pin <- function(name, ok) chk(if (PROVISIONAL_PINS) paste(name, "[provisional pin, not enforced]") else name, if (PROVISIONAL_PINS) TRUE else ok)
  s16 <- readLines(O("tab16_inmap_summary.txt")); num16 <- function(pat) as.numeric(sub(pat, "\\1", grep(pat, s16, value = TRUE)[1]))
  chk_pin("[operating] pw increment, district mean = 0.0594", near(num16(".*comparison:\\s+([0-9.]+) ug.*"), 0.0594, 0.01))
  chk_pin("[operating] pw increment, grid = 0.0593", near(num16(".*\\(grid\\): ([0-9.]+) ug.*"), 0.0593, 0.01))
  chk_pin("[operating] population >= 0.01 = 1,290 M", near(num16(".*>= 0.01 ug/m3 \\(grid cells\\): ([0-9.]+) million.*"), 1290, 0.01))
  chk_pin("[operating] GEMM deaths = 2,047", near(num16(".*increment: ([0-9.]+) /yr.*"), 2047, 0.01))
  d16 <- fread(O("tab16_inmap_district.csv")); chk("[operating] max district increment = Cuddalore 1.410", near(max(d16$dpm25), 1.410, 0.01) && grepl("Cuddalore", d16[which.max(dpm25), dist_name]))
  s16a <- readLines(A("tab16_inmap_summary.txt")); num16a <- function(pat) as.numeric(sub(pat, "\\1", grep(pat, s16a, value = TRUE)[1]))
  chk_pin("[all] pw increment, district mean = 0.2098", near(num16a(".*comparison:\\s+([0-9.]+) ug.*"), 0.2098, 0.01))
  chk_pin("[all] GEMM deaths = 7,260", near(num16a(".*increment: ([0-9.]+) /yr.*"), 7260, 0.01))
  nb <- fread(O("tab20b_inmap_disparity_national.csv"))
  chk_pin("20b SC/ST ratio 1.372, sign prob 0.059", near(nb[contrast == "SC/ST vs General", ratio], 1.372, 0.005) && near(nb[contrast == "SC/ST vs General", boot_sign_prob], 0.059, 0.2))
  chk_pin("20b rural/urban ratio 0.797, sign prob 0.028", near(nb[contrast == "Rural vs urban", ratio], 0.797, 0.005) && near(nb[contrast == "Rural vs urban", boot_sign_prob], 0.028, 0.2))
  rg <- fread(O("tab20b_inmap_regional_disparity.csv"))
  chk_pin("20b regional: North rural/urban Z -0.638, South SC/ST Z +0.481", near(rg[zone == "North" & contrast == "Rural vs urban", Z], -0.638, 0.01) && near(rg[zone == "South" & contrast == "SC/ST vs General", Z], 0.481, 0.01))
  bb <- fread(O("tab21_benefit_burden.csv"))
  chk_pin("21 Delhi ratio 10.35, Tamil Nadu 0.50, four states 47.8% of exposure", near(bb[state == "Nct Of Delhi", benefit_burden_ratio], 10.35, 0.01) && near(bb[state == "Tamil Nadu", benefit_burden_ratio], 0.502, 0.01) && near(bb[state == "Four use states", exposure_share], 0.478, 0.01))
  s23 <- readLines(O("tab23_monetised_summary.txt"))
  chk_pin("23 total India SCC $2.55 bn, $127/MWh (Ricke SCC rebased to 2020 US$, 19 Oct 2026)", any(grepl("total\\s+2\\.55 .*\\$127 ", s23)))
  s29 <- readLines(O("tab29_diesel_summary.txt"))
  chk_pin("29 diesel NOx 5.64% central, 5.26% zero-fill", any(grepl("5\\.64%", s29)) && any(grepl("5\\.26%", s29)))
  wd <- fread(O("tab13b_water_demand.csv")); chk("13b 144 of 233 facilities in extremely high stress", any(wd$n_ext_high == 144, na.rm = TRUE))
  t17 <- fread(O("tab17e_confidence_sensitivity.csv")); chk("17e high-confidence sensitivity (40 districts / 119 facilities)", nrow(t17) > 0 && t17$n_hosting_high[1] == 40 && t17$high_conf_facilities[1] == 119)
  # cross-file identities
  chk("district deaths sum to the summary total", near(sum(d16$deaths, na.rm = TRUE), num16(".*increment: ([0-9.]+) /yr.*"), 0.005))
  rr <- fread(O("tab20b_inmap_regional.csv")); chk("regional India row = district-mean increment", near(rr[zone == "India", pw_increment], num16(".*comparison:\\s+([0-9.]+) ug.*"), 0.005))
  mb <- fread(O("tab15b_marginal_totals.csv")); nat <- fread(O("tab15_national.csv"))[scenario == "central"]
  if ("e_twh" %in% names(mb)) chk("15b electricity = 15 electricity", near(mb$e_twh, nat$e_fac, 1e-6))
  pd <- fread(O("public/power_interruptions_backup_diesel.csv")); ds <- fread(O("tab29_diesel_scenarios.csv"))[abs(f - 0.4) < 1e-9 & abs(testing_h - 100) < 1e-9]
  chk("30 total row = 29 central NOx", near(pd[grepl("ALL HOSTING", district), diesel_nox_t_central][1], ds$diesel_nox_t[1], 1e-3))
}

# ---- 2c. dispersion sensitivity runs (run_sensitivity.R; Table S4.1.3, pinned 20 Oct 2026) ---------------------
# 26 Oct 2026: four variants (unresolved_zero added); the 20 Oct pins were for the 340-row inventory, so the numeric pins are
# NA (= report only) until the variants have been run on the corrected inventory, then pin them from tab16_inmap_summary_<run>.txt.
SENS_RUNS <- c(alloc_gen = 0.0594, w_flat = 0.1011, w_hyper_only = 0.0540, unresolved_zero = 0.0530); SENS_DEATHS <- c(alloc_gen = 2047, w_flat = 3416, w_hyper_only = 1872, unresolved_zero = 1824)   # pinned 29 Oct 2026
for (f in c("tab31_sensitivity_disparity.csv", "tab31_sensitivity_summary.csv", "tab31_sensitivity_summary.txt",
            paste0("tab16_inmap_summary_", names(SENS_RUNS), ".txt"), paste0("tab20b_inmap_disparity_national_", names(SENS_RUNS), ".csv")))
  add(sprintf("[sensitivity] %s", f), if (file.exists(O(f))) "PASS" else "MISSING")
if (INVENTORY == "v2" && all(file.exists(O(paste0("tab16_inmap_summary_", names(SENS_RUNS), ".txt"))))) {
  near <- function(x, g, tol) is.na(g) || (is.finite(x) && abs(x - g) <= tol * abs(g))   # NA pin = not yet pinned, passes
  `%+%` <- function(a, b) paste0(a, b)
  for (rn in names(SENS_RUNS)) {
    sr <- readLines(O(sprintf("tab16_inmap_summary_%s.txt", rn))); numr <- function(pat) as.numeric(sub(pat, "\\1", grep(pat, sr, value = TRUE)[1]))
    chk(sprintf("[sensitivity %s] stamped SENS=%s and converged", rn, rn), any(grepl(paste0("SENS=", rn), sr, fixed = TRUE)) && !any(grepl("NOT CONVERGED", sr)))
    chk(sprintf("[sensitivity %s] district-mean increment %s, deaths %s%s", rn, format(SENS_RUNS[[rn]]), format(SENS_DEATHS[[rn]]), if (is.na(SENS_RUNS[[rn]])) " (not yet pinned: now " %+% sprintf("%.4f / %.0f", numr(".*comparison:\\s+([0-9.]+) ug.*"), numr(".*increment: ([0-9.]+) /yr.*")) %+% ")" else ""),
        near(numr(".*comparison:\\s+([0-9.]+) ug.*"), SENS_RUNS[[rn]], 0.01) && near(numr(".*increment: ([0-9.]+) /yr.*"), SENS_DEATHS[[rn]], 0.01))
  }
  if (file.exists(O("tab31_sensitivity_summary.csv"))) {
    t31 <- fread(O("tab31_sensitivity_summary.csv")); nb <- fread(O("tab20b_inmap_disparity_national.csv"))   # wide: ratio_<run>, boot_sign_prob_<run>
    m <- merge(t31, nb[, .(contrast, ratio)], by = "contrast")
    chk("31 baseline column = 20b national ratios", nrow(m) == 10 && max(abs(m$ratio_baseline - m$ratio)) < 1e-6)
    chk("31 alloc_gen = baseline (GPPD generation is capacity x fuel CF)", max(abs(t31$ratio_alloc_gen - t31$ratio_baseline)) < 1e-3)
    for (rn in setdiff(names(SENS_RUNS), "alloc_gen")) if (paste0("ratio_", rn) %in% names(t31))
      chk(sprintf("31 %s max |ratio change| vs baseline = %.3f (pinned 29 Oct 2026: w_flat 0.158, w_hyper_only 0.048, unresolved_zero 0.013)", rn, max(abs(t31[[paste0("ratio_", rn)]] - t31$ratio_baseline))),
          abs(max(abs(t31[[paste0("ratio_", rn)]] - t31$ratio_baseline)) - c(w_flat = 0.158, w_hyper_only = 0.048, unresolved_zero = 0.013)[[rn]]) < 0.005)
  }
}

# ---- 3. headline snapshot ---------------------------------------------------------------------------
num <- function(f, pat) { L <- readLines(f); as.numeric(sub(pat, "\\1", grep(pat, L, value = TRUE)[1])) }
hv <- list(); H <- function(name, value, source) hv[[length(hv) + 1]] <<- data.table(name = name, value = value, source = source)
fr <- fread(path_frame)
H("operating facilities", nrow(io), "dc_current_inventory.csv"); H("all-scope facilities", nrow(ia), "outputs_all/dc_current_inventory.csv")
H("hosting districts (operating)", sum(fr$hosting), "analysis_district.csv"); H("hosting districts (all)", sum(fr$hosting_all), "analysis_district.csv")
for (sc in c("operating", "all")) {
  nat <- fread(outp("tab15_national.csv", sc))[scenario == "central"]; mb <- fread(outp("tab15b_marginal_totals.csv", sc))
  H(sprintf("[%s] IT MW (central)", sc), nat$total_mw, "tab15_national.csv"); H(sprintf("[%s] TWh", sc), nat$e_fac, "tab15_national.csv")
  H(sprintf("[%s] scope-1 water GL", sc), nat$scope1_gl, "tab15_national.csv"); H(sprintf("[%s] scope-2 water GL", sc), nat$scope2_gl, "tab15_national.csv")
  for (v in c("co2_mt", "so2_kt", "nox_kt", "pm25_kt")) H(sprintf("[%s] marginal %s", sc, v), mb[[v]], "tab15b_marginal_totals.csv")
  f <- outp("tab16_inmap_summary.txt", sc)
  H(sprintf("[%s] pw increment, grid", sc), num(f, ".*\\(grid\\): ([0-9.]+) ug.*"), "tab16_inmap_summary.txt")
  H(sprintf("[%s] pw increment, district mean", sc), num(f, ".*comparison:\\s+([0-9.]+) ug.*"), "tab16_inmap_summary.txt")
  H(sprintf("[%s] population >= 0.01 (M)", sc), num(f, ".*>= 0.01 ug/m3 \\(grid cells\\): ([0-9.]+) million.*"), "tab16_inmap_summary.txt")
  H(sprintf("[%s] GEMM deaths", sc), num(f, ".*increment: ([0-9.]+) /yr.*"), "tab16_inmap_summary.txt")
  ds <- fread(outp("tab29_diesel_scenarios.csv", sc))[abs(f - 0.4) < 1e-9 & abs(testing_h - 100) < 1e-9]
  H(sprintf("[%s] diesel NOx %% of grid (central)", sc), ds$pct_grid_nox, "tab29_diesel_scenarios.csv")
}
H("hosting-district ambient deaths", h$deaths_hosting, "tab13a_health_burden.csv")
H("facilities in policy states", fread(O("tab14_policy_burden.csv"))[has_policy == TRUE, sum(dc_count)], "tab14_policy_burden.csv")
t1 <- fread(O("tab1_dc_vs_nodc_profile.csv"))
for (v in c("Mean wealth score", "Urban share", "Relative Wealth Index", "Share SC/ST", "Surface NO2 (1 km annual)", "PM2.5, ACAG local"))
  if (v %in% t1$variable) H(sprintf("d all-India: %s", v), t1[variable == v, d_all_india], "tab1_dc_vs_nodc_profile.csv")
snap <- rbindlist(hv); snap[, run := format(Sys.time(), "%Y-%m-%d %H:%M")]
sf <- file.path(dir_logs, sprintf("headline_snapshot_%s.csv", format(Sys.Date(), "%Y%m%d")))
prev <- setdiff(sort(list.files(dir_logs, pattern = "^headline_snapshot_\\d{8}\\.csv$", full.names = TRUE)), sf)
fwrite(snap, sf)
if (length(prev)) {
  p <- fread(tail(prev, 1)); cmp <- merge(snap[, .(name, now = value)], p[, .(name, before = value)], by = "name", all = TRUE)
  cmp[, rel_change := (now - before) / abs(before)]
  moved <- cmp[is.na(rel_change) | abs(rel_change) > 1e-6]
  msg("compared with %s: %d of %d headline numbers moved", basename(tail(prev, 1)), nrow(moved), nrow(cmp))
  if (nrow(moved)) print(moved[order(-abs(rel_change))], digits = 5)
  fwrite(cmp, sub("\\.csv$", "_vs_previous.csv", sf))
}

# ---- 4. manifest -------------------------------------------------------------------------------------
fl <- c(list.files(file.path(ROOT, out_dirname("operating")), pattern = "\\.(csv|txt)$", full.names = TRUE, recursive = TRUE),
        list.files(file.path(ROOT, out_dirname("all")), pattern = "\\.(csv|txt)$", full.names = TRUE, recursive = TRUE))
fl <- fl[!grepl("run_logs", fl)]
fwrite(data.table(file = sub(paste0(ROOT, "/"), "", fl), md5 = unname(tools::md5sum(fl)), bytes = file.size(fl)),
       file.path(dir_logs, sprintf("output_manifest_%s.csv", format(Sys.Date(), "%Y%m%d"))))

r <- rbindlist(res); print(r[, .N, by = status])
bad <- r[status != "PASS"]; if (nrow(bad)) print(bad)
msg("snapshot: %s", sf)
if (nrow(bad)) quit(status = 1)
msg("check_pipeline: all %d checks passed", nrow(r))
