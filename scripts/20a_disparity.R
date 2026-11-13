# =============================================================================
# 20a_disparity.R  —  comparison (vi): population-weighted exposure disparity
# -----------------------------------------------------------------------------
# SI section S6, Tables S6.1 and S6.2. Unit: people, not places. For a social
# group g, each district is weighted by w_gd = population_d x share_gd (NFHS-5
# household-member share of the group). For an exposure x_d:
#   PWC_g = sum_d w_gd x_d / sum_d w_gd        (exposure of the average member)
#   Z     = (PWC_g - PWC_ref) / S_pooled        ("Sarkar-inspired" disparity)
#   S_pooled^2 = [(n_g - 1) v_g + (n_ref - 1) v_ref] / (n_g + n_ref - 2)
# with v the w-weighted variance of x across districts and n the Kish effective
# sample size (sum w)^2 / sum w^2. Strictly positive fields (PM2.5, NO2, LST)
# are log-transformed; zero-heavy fields (data-center presence) are not.
# Recovered 30 Sep 2026 by fitting candidate estimators to SI Table S6.2: this
# one reproduces all ten contrasts to ~0.01 for presence, PM2.5 and NO2.
#
# Wealth groups: population-weighted quintiles of DISTRICTS ranked by mean
# NFHS-5 wealth (Q1 poorest ... Q5 richest; reference = Q5).
# Uncertainty: 2,000-draw bootstrap resampling whole STATES (seeded) ->
# two-sided bootstrap sign probability (NOT a p-value; * when < 0.05); for
# data-center presence also 5,000 within-state permutations of the hosting
# labels (state hosting counts preserved), p = (1 + k) / (B + 1).
# Golden (v1 inventory; S6.2 presence column): SC/ST -0.23*, OBC -0.20, BPL -0.22*, NoElec
# -0.32*, Rural -0.69*, Muslim +0.02, Q1 -1.29*, Q2 -1.45*, Q3 -0.77*, Q4 -0.95*;
# P(hosting) Q1 2.8% vs Q5 45.2%, rural 7.2% vs urban 29.7%, SC/ST 11.9% vs General 20.1%.
# =============================================================================
source(file.path(Sys.getenv("DCEJ_ROOT", unset = (function(){a<-commandArgs(FALSE);f<-sub("^--file=","",grep("^--file=",a,value=TRUE));d<-if(length(f)) dirname(normalizePath(f[1])) else getwd();while(!file.exists(file.path(d,"scripts","00_config.R"))&&d!=dirname(d)) d<-dirname(d);d})()), "scripts", "00_config.R"))
B_BOOT <- as.integer(Sys.getenv("DCEJ_B_BOOT", "2000")); B_PERM <- as.integer(Sys.getenv("DCEJ_B_PERM", "5000")); SEED <- 20260901

fr <- fread(path_frame)
host <- if (DC_SCOPE == "operating") "hosting" else "hosting_all"
n_host0 <- fr[get(host) == 1, .N]
fr <- fr[!is.na(pop_2020) & pop_2020 > 0 & !is.na(wealth_mean_2019)]
msg("districts with population and NFHS-5 wealth: %d of %d; hosting retained %d of %d%s", nrow(fr), N_DIST, fr[get(host) == 1, .N], n_host0,
    if (fr[get(host) == 1, .N] < n_host0) paste0(" (dropped: ", paste(setdiff(fread(path_frame)[get(host) == 1, dist_name], fr[get(host) == 1, dist_name]), collapse = ", "), ")") else "")
fr[, `:=`(presence = as.numeric(get(host)), rural = 1 - urban_share_2019, nonbpl = 1 - share_bpl_2019,
          haselec = 1 - share_no_electricity_2019, st2015 = state_name_2015)]
setorder(fr, wealth_mean_2019); fr[, cp := cumsum(pop_2020) / sum(pop_2020)]
fr[, wq := pmin(5L, as.integer(ceiling(cp * 5 - 1e-9)))]
for (q in 1:5) fr[, (paste0("wq", q)) := as.numeric(wq == q)]

contrasts <- data.table(
  label = c("SC/ST vs General", "OBC vs General", "BPL vs non-BPL", "No electricity vs has electricity", "Rural vs urban",
            "Muslim vs Hindu", "Poorest quintile vs richest", "Poorer quintile vs richest", "Middle quintile vs richest", "Richer quintile vs richest"),
  g   = c("share_scst_2019", "share_obc_2019", "share_bpl_2019", "share_no_electricity_2019", "rural", "share_muslim_2019", "wq1", "wq2", "wq3", "wq4"),
  ref = c("share_general_2019", "share_general_2019", "nonbpl", "haselec", "urban_share_2019", "share_hindu_2019", "wq5", "wq5", "wq5", "wq5"))
exposures <- data.table(var = c("presence", "pm25_acag_local", "no2_surf", "lst_day_c", "o3_summer"),
                        name = c("Data-center presence", "PM2.5 (ACAG)", "Surface NO2", "Land-surface temp. (MODIS)", "Ozone (summer, 2013-2022)"),
                        log = c(FALSE, TRUE, TRUE, TRUE, TRUE))
exposures <- exposures[var %in% names(fr)]

wmom <- function(x, w) { k <- !is.na(x) & w > 0; x <- x[k]; w <- w[k]; W <- sum(w); m <- sum(w * x) / W
  c(m = m, v = sum(w * (x - m)^2) / W, n = W^2 / sum(w^2)) }
zstat <- function(x, wg, wr) { a <- wmom(x, wg); b <- wmom(x, wr)
  s <- sqrt(((a[["n"]] - 1) * a[["v"]] + (b[["n"]] - 1) * b[["v"]]) / (a[["n"]] + b[["n"]] - 2))
  if (!is.finite(s) || s == 0) return(NA_real_); (a[["m"]] - b[["m"]]) / s }
pwc <- function(x, w) { k <- !is.na(x) & w > 0; sum(w[k] * x[k]) / sum(w[k]) }
xval <- function(d, e) { x <- d[[e$var]]; if (e$log) log(pmax(x, 1e-12)) else x }

# ---- point estimates ----------------------------------------------------------------------
res <- rbindlist(lapply(seq_len(nrow(exposures)), function(i) { e <- exposures[i]
  rbindlist(lapply(seq_len(nrow(contrasts)), function(j) { cc <- contrasts[j]
    wg <- fr$pop_2020 * fr[[cc$g]]; wr <- fr$pop_2020 * fr[[cc$ref]]
    data.table(exposure = e$name, contrast = cc$label, pwc_group = pwc(fr[[e$var]], wg), pwc_ref = pwc(fr[[e$var]], wr),
               Z = zstat(xval(fr, e), wg, wr)) })) }))
res[, ratio := pwc_group / pwc_ref]

# ---- state bootstrap (sign probability) --------------------------------------------------
set.seed(SEED); states <- unique(fr$st2015); idx_by_state <- split(seq_len(nrow(fr)), fr$st2015)
boot <- replicate(B_BOOT, {
  s <- sample(states, length(states), replace = TRUE); ii <- unlist(idx_by_state[s], use.names = FALSE); d <- fr[ii]
  unlist(lapply(seq_len(nrow(exposures)), function(i) { e <- exposures[i]; x <- xval(d, e)
    vapply(seq_len(nrow(contrasts)), function(j) zstat(x, d$pop_2020 * d[[contrasts$g[j]]], d$pop_2020 * d[[contrasts$ref[j]]]), 0) }))
})
res[, boot_sign_prob := apply(boot, 1, function(z) { z <- z[is.finite(z)]; 2 * min((1 + sum(z <= 0)) / (length(z) + 1), (1 + sum(z >= 0)) / (length(z) + 1)) })]
res[, boot_sign_prob := pmin(1, boot_sign_prob)]
res[, star := fifelse(boot_sign_prob < 0.05, "*", "")]

# ---- within-state permutation for presence ------------------------------------------------
set.seed(SEED + 1)
perm_idx <- replicate(B_PERM, { p <- fr$presence; for (ii in idx_by_state) if (length(ii) > 1) p[ii] <- p[ii][sample.int(length(ii))]; p })
pp <- vapply(seq_len(nrow(contrasts)), function(j) {
  wg <- fr$pop_2020 * fr[[contrasts$g[j]]]; wr <- fr$pop_2020 * fr[[contrasts$ref[j]]]
  z0 <- zstat(fr$presence, wg, wr); zb <- apply(perm_idx, 2, function(p) zstat(p, wg, wr))
  (1 + sum(abs(zb) >= abs(z0) - 1e-12)) / (B_PERM + 1) }, 0)
res[exposure == "Data-center presence", perm_p_within_state := pp]
fwrite(res, out("tab20a_disparity_national.csv"))

wide <- dcast(res, contrast ~ exposure, value.var = "Z")
setcolorder(wide, c("contrast", intersect(exposures$name, names(wide))))
wide <- wide[match(contrasts$label, contrast)]
print(wide, digits = 2)
pres <- res[exposure == "Data-center presence"]
msg("P(hosting): poorest %.1f%% vs richest %.1f%%; rural %.1f%% vs urban %.1f%%; SC/ST %.1f%% vs General %.1f%%",
    100 * pres[contrast == "Poorest quintile vs richest", pwc_group], 100 * pres[contrast == "Poorest quintile vs richest", pwc_ref],
    100 * pres[contrast == "Rural vs urban", pwc_group], 100 * pres[contrast == "Rural vs urban", pwc_ref],
    100 * pres[contrast == "SC/ST vs General", pwc_group], 100 * pres[contrast == "SC/ST vs General", pwc_ref])

# ---- Table S6.1: group-weighted exposure relative to the average Indian ------------------------
groups <- c(`General caste` = "share_general_2019", `Scheduled Caste` = "share_sc_2019", `Scheduled Tribe` = "share_st_2019",
            OBC = "share_obc_2019", Muslim = "share_muslim_2019", `Below poverty line` = "share_bpl_2019", `Above poverty line` = "nonbpl")
ex61 <- c(presence = "P(hosting)", pm25_acag_local = "PM2.5", no2_surf = "Surface NO2", hot_days_peryr = "Hot days")
ex61 <- ex61[names(ex61) %in% names(fr)]
nat <- sapply(names(ex61), function(v) pwc(fr[[v]], fr$pop_2020))
t61 <- rbindlist(lapply(names(groups), function(g) { w <- fr$pop_2020 * fr[[groups[[g]]]]
  c(list(group = g, population_million = sum(w, na.rm = TRUE) / 1e6),
    setNames(lapply(names(ex61), function(v) pwc(fr[[v]], w) / nat[[v]]), unname(ex61))) }))
print(t61, digits = 3)
fwrite(t61, out("tab20a_group_exposure_S61.csv"))

if (DC_SCOPE == "operating" && is_v1) {   # SI Table S6.2 of the submitted paper (v1 inventory)
  gold <- c(-0.23, -0.20, -0.22, -0.32, -0.69, 0.02, -1.29, -1.45, -0.77, -0.95)
  d <- max(abs(pres$Z - gold)); msg("presence Z vs SI Table S6.2: max |diff| = %.3f", d)
  assert(d < 0.05, "presence disparity no longer reproduces SI Table S6.2")
}
