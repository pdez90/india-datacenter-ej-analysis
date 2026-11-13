# =============================================================================
# 14_policy_table.R  —  state data-center incentive policies vs hosting burden
# -----------------------------------------------------------------------------
# Inputs:
#   data/state_dc_policies.csv          18 states (13 dedicated policies + Delhi,
#                                        Kerala, Punjab, MP, Chhattisgarh), verified
#                                        1 Sep 2026. Reseeded from the app layer.
#   data/state_dc_policies_sources.csv  one row per state x claim with URL,
#                                        verbatim quote and retrieval date (140
#                                        sourced rows; the 18 unsourced
#                                        placeholder rows of the lost original
#                                        were not carried by the app).
# Rules (bug fixes kept): has_policy via startsWith(policy_name, "none") is FALSE;
# Telangana/AP split at district level; a claim counts as sourced only with url +
# quote + retrieved + non-blank value; STRICT mode stops on unsourced claims.
# Per-state columns (hosting districts of that state): mean ACAG PM2.5
# (unweighted), median baseline water stress, share of hosting districts with
# BWS >= 0.4 ("high or worse"), median groundwater-table decline, mean TROPOMI
# NO2 column (x 1e5). Columns whose inputs are not regenerated yet come out NA.
# Golden (v1 inventory, operating): 13 policy states; 182 of 209 facilities (87%); policy-state
# hosting districts in high water stress 17 of 23 (district-weighted).
# =============================================================================
source(file.path(Sys.getenv("DCEJ_ROOT", unset = (function(){a<-commandArgs(FALSE);f<-sub("^--file=","",grep("^--file=",a,value=TRUE));d<-if(length(f)) dirname(normalizePath(f[1])) else getwd();while(!file.exists(file.path(d,"scripts","00_config.R"))&&d!=dirname(d)) d<-dirname(d);d})()), "scripts", "00_config.R"))
POLICY_SOURCES_STRICT <- TRUE

pol <- fread(file.path(dir_data, "state_dc_policies.csv"))
src <- fread(file.path(dir_data, "state_dc_policies_sources.csv"))
pol[, has_policy := !startsWith(tolower(policy_name), "none")]
assert(pol[has_policy == TRUE, .N] == 13, "expected 13 dedicated-policy states")

# ---- provenance -------------------------------------------------------------------------
src[, sourced := nzchar(url) & nzchar(quote) & nzchar(retrieved) & nzchar(as.character(value))]
claims <- c("capital_subsidy","stamp_duty_exemption","electricity_duty_exemption","land_incentive","single_window","eia_required")
long <- melt(pol[, c("state", claims), with = FALSE], id.vars = "state", variable.name = "claim", value.name = "value")
chk <- merge(long, src[, .(state, claim, sourced)], by = c("state", "claim"), all.x = TRUE)
uns <- chk[is.na(sourced) | sourced == FALSE]
fwrite(uns, out("tab14_policy_unsourced.csv"))
if (POLICY_SOURCES_STRICT) {
  assert(!nrow(uns), "%d policy claims lack a source row (see tab14_policy_unsourced.csv)", nrow(uns))
  assert(!any(startsWith(toupper(pol$verify_note), "VERIFY")), "a policy row is still marked VERIFY")
}
fwrite(src[, .(n_claims = .N, n_sourced = sum(sourced)), by = state], out("tab14b_policy_provenance.csv"))

# ---- hosting-district burden by state ----------------------------------------------------------
fr  <- fread(path_frame)
fr[, state_key := fifelse(state_name == "Nct Of Delhi", "Delhi (NCT)", fifelse(state_name == "Chattisgarh", "Chhattisgarh", state_name))]
host_col <- if (DC_SCOPE == "operating") "dc_count" else "dc_count_all"
h <- fr[get(host_col) > 0]
nz <- function(v) if (v %in% names(h)) h[[v]] else rep(NA_real_, nrow(h))
h[, `:=`(gtd = nz("gtd_raw"), no2c = nz("no2_column"))]
agg <- h[, .(dc_districts = .N, dc_count = sum(get(host_col)), median_bws = median(bws_raw, na.rm = TRUE),
             share_dists_high_stress = mean(bws_raw >= 0.4, na.rm = TRUE), median_gtd = median(gtd, na.rm = TRUE),
             mean_pm25 = mean(pm25_acag_local, na.rm = TRUE), mean_no2_1e5 = mean(no2c, na.rm = TRUE) * 1e5), by = .(state = state_key)]
tab <- merge(pol, agg, by = "state", all.x = TRUE)
tab[is.na(dc_count), `:=`(dc_count = 0L, dc_districts = 0L)]
setorder(tab, -has_policy, policy_year)
fwrite(tab, out("tab14_policy_burden.csv"))

n_tot <- sum(fr[[host_col]]); n_pol <- tab[has_policy == TRUE, sum(dc_count)]
hs <- h[state_key %in% tab[has_policy == TRUE, state]]
msg("%d of %d facilities (%.0f%%) in %d dedicated-policy states", n_pol, n_tot, 100 * n_pol / n_tot, tab[has_policy == TRUE, .N])
msg("policy-state hosting districts with BWS >= 0.4: %d of %d; mean hosting-district PM2.5 %.1f",
    hs[bws_raw >= 0.4, .N], nrow(hs), hs[, mean(pm25_acag_local)])
unassigned <- setdiff(unique(h$state_key), pol$state)
if (length(unassigned)) msg("hosting states without a policy row: %s", paste(unassigned, collapse = ", "))
if (DC_SCOPE == "operating") assert(n_pol == G(182, 200), "expected %d facilities in policy states, got %d", G(182, 200), n_pol)
if (DC_SCOPE == "all") assert(n_pol == G(267, 293), "expected %d facilities in policy states (all scope), got %d", G(267, 293), n_pol)   # v2 pinned 4 Oct 2026
