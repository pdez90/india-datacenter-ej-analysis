# =============================================================================
# 10_permutation.R  —  within-state permutation inference (SI Table S3.1)
# -----------------------------------------------------------------------------
# Observed statistic: state-demeaned hosting minus non-hosting difference
# within the hosting states (2015 labels; 25 under the v2 inventory). Null: hosting labels re-randomized
# 5,000 times WITHIN states, preserving each state's number of hosting
# districts (fixed seed). Two-sided p = (1 + #|perm| >= |obs|) / (B + 1); per-
# variable complete cases; Benjamini-Hochberg adjustment across the variable set.
# Golden (v1 inventory, submitted Table S3.1; v2 values in tab10b): wealth +450,970 p<.001; urban +0.267; RWI +0.236;
# SC/ST -0.065 p=.002; BPL -0.098; no electricity -0.014 p=.006; surface NO2
# +1.08 p=.001; PM2.5 ACAG +3.0 p=.019; hot days -4.3 p=.29.
# =============================================================================
source(file.path(Sys.getenv("DCEJ_ROOT", unset = (function(){a<-commandArgs(FALSE);f<-sub("^--file=","",grep("^--file=",a,value=TRUE));d<-if(length(f)) dirname(normalizePath(f[1])) else getwd();while(!file.exists(file.path(d,"scripts","00_config.R"))&&d!=dirname(d)) d<-dirname(d);d})()), "scripts", "00_config.R"))
B <- as.integer(Sys.getenv("DCEJ_B_PERM", "5000")); set.seed(20260901)

fr <- fread(path_frame)
if (DC_SCOPE == "all") fr[, hosting := hosting_all]
fr[, st := state_name_2015]
g <- fr[st %in% fr[hosting == 1, unique(st)]]
VARS <- c(wealth_mean_2019 = "Mean wealth score", urban_share_2019 = "Urban share", rwi_mean = "Relative Wealth Index",
          share_scst_2019 = "Share SC/ST", share_bpl_2019 = "Share BPL", share_no_electricity_2019 = "Share without electricity",
          no2_surf = "Surface NO2 (1 km)", o3_annual = "Surface ozone, annual", o3_summer = "Surface ozone, summer (JJA)",
          no2_column = "NO2 column (S5P)", pm25_acag_local = "PM2.5 (ACAG)", pm25_lghap = "PM2.5 (LGHAP)",
          hcho_column = "HCHO column (S5P)", t2m_mean_c = "Mean air temperature (ERA5-Land)",
          lst_day_c = "Daytime land-surface temperature (MODIS)", hot_days_peryr_era5 = "Hot days per year")
VARS <- VARS[names(VARS) %in% names(g)]

res <- rbindlist(lapply(names(VARS), function(v) {
  d <- g[!is.na(get(v))]; x <- d[[v]] - ave(d[[v]], d$st, FUN = mean); h <- d$hosting
  obs <- mean(x[h == 1]) - mean(x[h == 0])
  idx <- split(seq_len(nrow(d)), d$st)
  perm <- replicate(B, { p <- h; for (ii in idx) if (length(ii) > 1) p[ii] <- p[ii][sample.int(length(ii))]; mean(x[p == 1]) - mean(x[p == 0]) })
  data.table(variable = VARS[[v]], observed_difference = obs, permutation_p = (1 + sum(abs(perm) >= abs(obs) - 1e-12)) / (B + 1), n = nrow(d)) }))
res[, bh_p := p.adjust(permutation_p, "BH")]
print(res, digits = 3)
fwrite(res, out("tab10b_permutation.csv"))
if (DC_SCOPE == "operating") assert(res[variable == "Surface NO2 (1 km)", permutation_p] <= 0.01, "surface NO2 permutation p should be <= 0.01 (published 0.001)")
