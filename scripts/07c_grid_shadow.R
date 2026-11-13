# =============================================================================
# 07c_grid_shadow.R  —  data-center districts vs coal-generation districts
# -----------------------------------------------------------------------------
# Main-text Table 1 and SI Table S8.1. Coal districts hold >= 1 GW of
# district-assigned coal capacity (WRI GPPD). Four-way split of the 641
# districts: DC only (54) / coal only (62) / both (7) / neither (518) under the
# operating v2 inventory. Rows are UNWEIGHTED district means; p compares DC-only vs
# coal-only districts with Welch's t-test (reproduces the published p-values).
# Also reports the share of district-assigned coal capacity inside hosting
# districts (published: 13.9 GW, 6.8%; 93% outside).
# =============================================================================
source(file.path(Sys.getenv("DCEJ_ROOT", unset = (function(){a<-commandArgs(FALSE);f<-sub("^--file=","",grep("^--file=",a,value=TRUE));d<-if(length(f)) dirname(normalizePath(f[1])) else getwd();while(!file.exists(file.path(d,"scripts","00_config.R"))&&d!=dirname(d)) d<-dirname(d);d})()), "scripts", "00_config.R"))

fr <- fread(path_frame)
# Sentinel-5P SO2 column (03, mol/m2, 2019-2024 mean) in the table's 1e-4 mol/m2 units.
# Rebuild 30 Sep 2026: DC only 1.23, coal only 1.72, both 1.36, p .008 (published 1.21 / 1.69 / 1.34, p .010).
if ("so2_column" %in% names(fr)) fr[, so2_column_1e4 := so2_column * 1e4]
if (DC_SCOPE == "all") { fr[, hosting := hosting_all]
  fr[, class4 := fcase(hosting == 1 & coal_district == 0, "DC only", hosting == 0 & coal_district == 1, "coal only",
                       hosting == 1 & coal_district == 1, "both", default = "neither")] }

rows <- list(
  c("pop_2020",                   "Population, millions (2020)",  1e-6, "sum"),
  c("wealth_mean_2019",           "Mean wealth score (NFHS-5)",   1,    "mean"),
  c("rwi_mean",                   "Relative Wealth Index",        1,    "mean"),
  c("urban_share_2019",           "Urban share",                  1,    "mean"),
  c("share_scst_2019",            "Share SC/ST",                  1,    "mean"),
  c("share_st_2019",              "Share Scheduled Tribe",        1,    "mean"),
  c("share_bpl_2019",             "Share BPL card",               1,    "mean"),
  c("share_no_electricity_2019",  "Share without electricity",    1,    "mean"),
  c("pm25_acag_local",            "PM2.5, ACAG (ug/m3)",          1,    "mean"),
  c("so2_column_1e4",             "SO2 column (10-4 mol/m2)",     1,    "mean"),
  c("coal_mw",                    "Coal capacity (mean MW)",      1,    "mean"))
grp <- c("DC only", "coal only", "both")
tab <- rbindlist(lapply(rows, function(r) {
  v <- r[1]; if (!v %in% names(fr)) return(data.table(variable = r[2], `DC only` = NA_real_, `coal only` = NA_real_, both = NA_real_, p_dc_vs_coal = NA_real_, note = "missing: run 03 (Earth Engine) then 06"))
  f <- if (r[4] == "sum") function(x) sum(x, na.rm = TRUE) * as.numeric(r[3]) else function(x) mean(x, na.rm = TRUE) * as.numeric(r[3])
  s <- sapply(grp, function(k) f(fr[class4 == k][[v]]))
  a <- fr[class4 == "DC only"][[v]]; b <- fr[class4 == "coal only"][[v]]
  p <- if (r[4] == "mean" && v != "coal_mw") t.test(a, b)$p.value else NA_real_
  data.table(variable = r[2], `DC only` = s[1], `coal only` = s[2], both = s[3], p_dc_vs_coal = p, note = "")
}))
tab <- rbind(data.table(variable = "Districts (n)", `DC only` = fr[class4 == "DC only", .N], `coal only` = fr[class4 == "coal only", .N],
                        both = fr[class4 == "both", .N], p_dc_vs_coal = NA_real_, note = ""), tab)
print(tab)
fwrite(tab, out("tab7c_dc_vs_coal.csv"))

coal_in_host <- fr[hosting == 1, sum(coal_mw)]; coal_tot <- fr[, sum(coal_mw)]
shadow <- data.table(scope = DC_SCOPE, n_dc_only = fr[class4 == "DC only", .N], n_coal_only = fr[class4 == "coal only", .N],
                     n_both = fr[class4 == "both", .N], n_neither = fr[class4 == "neither", .N],
                     coal_mw_total = coal_tot, coal_mw_in_hosting = coal_in_host, pct_coal_in_hosting = 100 * coal_in_host / coal_tot,
                     pop_coal_only_million = fr[class4 == "coal only", sum(pop_2020, na.rm = TRUE)] / 1e6)
print(shadow); fwrite(shadow, out("tab7c_grid_shadow.csv"))
if (DC_SCOPE == "operating") assert(shadow$n_dc_only == G(26, 54) && shadow$n_coal_only == G(64, 62) && shadow$n_both == G(5, 7), "four-way split must be %d/%d/%d", G(26, 54), G(64, 62), G(5, 7))   # v2 pinned 4 Oct 2026
