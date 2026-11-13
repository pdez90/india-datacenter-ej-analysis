# =============================================================================
# 16e_group_exposure.R  —  who breathes the InMAP increment: group exposure relative to the national mean
# -----------------------------------------------------------------------------
# For each group g, the population-weighted mean of the district increment dpm25 (from 16d), weighting
# district d by pop_2020_d x share_gd, divided by the national population-weighted mean:
#   ratio_g = [sum_d w_gd dpm25_d / sum_d w_gd] / [sum_d pop_d dpm25_d / sum_d pop_d]
# Groups: SC/ST, BPL, urban, rural, Muslim, no electricity, and district wealth quintiles. The wealth
# quintiles here are EQUAL-COUNT quintiles of districts ranked by NFHS-5 mean wealth (all districts
# with a wealth value), as in the submitted paper's Figure 2 panel and Table S9; 20b uses
# population-weighted quintiles for the disparity contrasts.
# Self-test: on the submitted paper's district increments (data/golden/districts.geojson, run 3) the
# ratios must reproduce SC/ST 1.015, BPL 1.017, urban 1.189, rural 0.912, Q1-Q5 0.569 / 0.598 / 1.038 /
# 1.570 / 1.022 (to 0.003).
# Output: tab16e_group_exposure.csv
# =============================================================================
source(file.path(Sys.getenv("DCEJ_ROOT", unset = (function(){a<-commandArgs(FALSE);f<-sub("^--file=","",grep("^--file=",a,value=TRUE));d<-if(length(f)) dirname(normalizePath(f[1])) else getwd();while(!file.exists(file.path(d,"scripts","00_config.R"))&&d!=dirname(d)) d<-dirname(d);d})()), "scripts", "00_config.R"))
suppressMessages(library(sf))

pwc <- function(x, w) { k <- !is.na(x) & !is.na(w) & w > 0; sum(w[k] * x[k]) / sum(w[k]) }
group_table <- function(fr, inc) {
  fr <- copy(fr)
  br <- quantile(fr$wealth_mean_2019, 0:5 / 5, na.rm = TRUE)          # breaks over all districts with a wealth value
  fr[, wq := cut(wealth_mean_2019, br, include.lowest = TRUE, labels = FALSE)]
  fr <- fr[!is.na(pop_2020) & pop_2020 > 0]
  fr[, rural := 1 - urban_share_2019]
  for (q in 1:5) fr[, (paste0("wq", q)) := as.numeric(!is.na(wq) & wq == q)]
  nat <- pwc(fr[[inc]], fr$pop_2020)
  grp <- c(`SC/ST` = "share_scst_2019", `Scheduled Caste` = "share_sc_2019", `Scheduled Tribe` = "share_st_2019",
           BPL = "share_bpl_2019", Muslim = "share_muslim_2019", `No electricity` = "share_no_electricity_2019",
           Urban = "urban_share_2019", Rural = "rural", `Q1 (poorest)` = "wq1", Q2 = "wq2", Q3 = "wq3", Q4 = "wq4", `Q5 (richest)` = "wq5")
  rbindlist(lapply(names(grp), function(n) { w <- fr$pop_2020 * fr[[grp[[n]]]]
    data.table(group = n, population_million = sum(w, na.rm = TRUE) / 1e6, mean_increment_ugm3 = pwc(fr[[inc]], w),
               national_mean_ugm3 = nat, ratio_to_national = pwc(fr[[inc]], w) / nat) }))
}

# ---- self-test on run 3 ----------------------------------------------------------------------
gold_path <- file.path(dir_data, "golden", "districts.geojson")
if (DC_SCOPE == "operating" && file.exists(gold_path) && file.exists(file.path(dir_derived, "analysis_district_v1.csv"))) {
  g <- as.data.table(st_drop_geometry(st_read(gold_path, quiet = TRUE)))[, .(zone_uid, dpm25)]
  f1 <- merge(fread(file.path(dir_derived, "analysis_district_v1.csv")), g, by = "zone_uid")
  t1 <- group_table(f1, "dpm25")
  want <- c(`SC/ST` = 1.015, BPL = 1.017, Urban = 1.189, Rural = 0.912, `Q1 (poorest)` = 0.569, Q2 = 0.598, Q3 = 1.038, Q4 = 1.570, `Q5 (richest)` = 1.022)
  got <- setNames(t1$ratio_to_national, t1$group)[names(want)]
  print(round(rbind(got, want), 3))
  msg("self-test vs submitted paper (run 3): max |diff| %.4f", max(abs(got - want)))
  assert(max(abs(got - want)) < 0.003, "16e no longer reproduces the run-3 group ratios")
}

# ---- this run ----------------------------------------------------------------------------------
d16 <- fread(out("tab16_inmap_district.csv"))[, .(zone_uid, dpm25)]
fr <- merge(fread(path_frame), d16, by = "zone_uid")
t <- group_table(fr, "dpm25")
print(t[, .(group, population_million = round(population_million), mean = signif(mean_increment_ugm3, 3), ratio = round(ratio_to_national, 3))])
fwrite(cbind(scope = DC_SCOPE, inventory = INVENTORY, t), out("tab16e_group_exposure.csv"))
msg("16e done")
