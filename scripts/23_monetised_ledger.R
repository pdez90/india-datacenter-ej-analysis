# =============================================================================
# 23_monetised_ledger.R  —  partial monetised ledger, cost side only (SI S11, Table S11.1)
# -----------------------------------------------------------------------------
# health = GEMM deaths (16d) x VSL;  carbon = marginal CO2 (15b) x social cost of carbon;
# total = health + carbon; also per MWh consumed (15b e_twh) and per MW-year of IT load (15 central MW).
# Parameters (2020 US$):
#   VSL low 0.300 M (Viscusi & Masterman 2017, India 0.275 M 2015$, CPI-U 237.017 -> 258.811);
#   VSL high 0.690 M (Majumder & Madheswaran 2018, ISEC WP 407, INR 44.69 M = US$0.64 M 2016-17,
#   CPI-U 240.007 -> 258.811); central = midpoint 0.495 M.
#   India country-level SCC: Ricke et al. 2018, 86 [49-157] $/t (66% interval) in 2010 US$ (their code converts its USD2005
#   output to USD2010 with factor 1.10774); rebased to 2020 US$ with CPI-U 218.056 -> 258.811 (x 1.1869): 102.1 [58.2-186.3].
#   (Rebased 19 Oct 2026; before that the values were used as published.)
#   Global SC-CO2: EPA 2023 Table ES.1, 2020 emissions, 190 (2.0%) [120 (2.5%) - 340 (1.5%)] $/t.
# Water (scope 1 and 2) and diesel NOx are listed, unvalued, where their tables exist.
# Each scope is costed only if its InMAP summary (tab16_inmap_summary.txt) exists and matches it.
# Golden (submitted paper, operating: 1,976 deaths, 14.65 Mt, 19.7 TWh, 2,005 MW): health 0.98 [0.59-1.36]
#   bn; carbon 1.26 [0.72-2.30] (India) / 2.78 [1.76-4.98] (EPA); total India 2.24 [1.31-3.66] bn =
#   $114 [67-186]/MWh, $1.12 M/MW-yr. Reproduced as a self-test from those inputs.
# Outputs (outputs/ of the active inventory): tab23_monetised_ledger.csv, tab23_ledger_parameters.csv,
#   tab23_monetised_summary.txt
# =============================================================================
source(file.path(Sys.getenv("DCEJ_ROOT", unset = (function(){a<-commandArgs(FALSE);f<-sub("^--file=","",grep("^--file=",a,value=TRUE));d<-if(length(f)) dirname(normalizePath(f[1])) else getwd();while(!file.exists(file.path(d,"scripts","00_config.R"))&&d!=dirname(d)) d<-dirname(d);d})()), "scripts", "00_config.R"))

CPI_2010_2020 <- 258.811 / 218.056     # US CPI-U annual averages
P_pub <- data.table(parameter = c("vsl_musd", "scc_india", "scc_epa"),
                low = c(0.300, 49, 120), central = c(0.495, 86, 190), high = c(0.690, 157, 340))
P <- data.table(parameter = c("vsl_musd", "scc_india", "scc_epa"),
                low = c(0.300, 49 * CPI_2010_2020, 120), central = c(0.495, 86 * CPI_2010_2020, 190), high = c(0.690, 157 * CPI_2010_2020, 340),
                source = c("Viscusi & Masterman 2017 (low); Majumder & Madheswaran 2018 (high); midpoint (central); 2020 US$",
                           "Ricke et al. 2018 country-level SCC, India; 86 [49-157] in 2010 US$, rebased to 2020 US$ with the CPI-U",
                           "US EPA 2023 Report on the SC-GHG, Table ES.1, 2020 emissions, 2.5% / 2.0% / 1.5%; 2020 US$"))
ledger <- function(scope, deaths, co2_mt, twh, mw, P = get("P", envir = globalenv())) {
  rbindlist(lapply(c("low", "central", "high"), function(lv) {
    vsl <- P[parameter == "vsl_musd"][[lv]]
    rbindlist(lapply(c("india", "epa"), function(b) { scc <- P[parameter == paste0("scc_", b)][[lv]]
      h <- deaths * vsl / 1e3; cbn <- co2_mt * scc / 1e3
      data.table(scope = scope, basis = b, level = lv, term = c("health", "carbon", "total"), usd_bn = c(h, cbn, h + cbn)) })) }))[
    , `:=`(usd_per_mwh = usd_bn * 1e9 / (twh * 1e6), usd_per_mw_yr = usd_bn * 1e9 / mw)][]
}

# ---- self-test: submitted paper ----------------------------------------------------------------
t0 <- ledger("operating", 1976, 14.6486, 19.6684, 2005, P = P_pub)   # golden uses the as-published values
g <- t0[basis == "india" & term == "total"]
msg("self-test (submitted-paper inputs): total %.2f [%.2f-%.2f] bn, $%.0f/MWh, $%.2f M/MW-yr",
    g[level == "central", usd_bn], g[level == "low", usd_bn], g[level == "high", usd_bn], g[level == "central", usd_per_mwh], g[level == "central", usd_per_mw_yr] / 1e6)
assert(abs(g[level == "central", usd_bn] - 2.24) < 0.01 && abs(g[level == "low", usd_bn] - 1.31) < 0.01 &&
       abs(g[level == "high", usd_bn] - 3.66) < 0.01 && abs(g[level == "central", usd_per_mwh] - 114) < 1, "23 no longer reproduces Table S11.1")

# ---- this inventory, each scope with an InMAP summary -------------------------------------------
read_deaths <- function(scope) {
  f <- outp("tab16_inmap_summary.txt", scope); if (!file.exists(f)) return(NA_real_)
  L <- readLines(f)
  if (!any(grepl(sprintf("DC_SCOPE=%s, INVENTORY=%s", scope, INVENTORY), L, fixed = TRUE))) return(NA_real_)
  as.numeric(sub(".*: ([0-9.]+) /yr.*", "\\1", grep("GEMM attributable deaths", L, value = TRUE)[1]))
}
res <- list(); summ <- character()
for (sc in c("operating", "all")) {
  deaths <- read_deaths(sc)
  if (is.na(deaths)) { msg("scope %s: no InMAP summary for INVENTORY=%s (run 16b-16c-16d for it); not costed", sc, INVENTORY); next }
  tot <- fread(outp("tab15b_marginal_totals.csv", sc))
  nat <- fread(outp("tab15_national.csv", sc))[scenario == "central"]
  r <- ledger(sc, deaths, tot$co2_mt, tot$e_twh, nat$total_mw)
  res[[sc]] <- r
  f <- function(b, tm) { x <- r[basis == b & term == tm]; sprintf("%.2f [%.2f-%.2f] bn", x[level == "central", usd_bn], x[level == "low", usd_bn], x[level == "high", usd_bn]) }
  pm <- function(b) { x <- r[basis == b & term == "total"]; sprintf("$%.0f [%.0f-%.0f]/MWh, $%.2f M/MW-yr", x[level == "central", usd_per_mwh], x[level == "low", usd_per_mwh], x[level == "high", usd_per_mwh], x[level == "central", usd_per_mw_yr] / 1e6) }
  summ <- c(summ, sprintf("%s (INVENTORY=%s): %.0f deaths, %.2f Mt CO2, %.1f TWh, %.0f MW", sc, INVENTORY, deaths, tot$co2_mt, tot$e_twh, nat$total_mw),
            sprintf("  health %s", f("india", "health")),
            sprintf("  carbon %s (India SCC) | %s (EPA)", f("india", "carbon"), f("epa", "carbon")),
            sprintf("  total  %s = %s (India SCC)", f("india", "total"), pm("india")),
            sprintf("  total  %s = %s (EPA)", f("epa", "total"), pm("epa")),
            sprintf("  unvalued: scope-1 water %.1f GL, scope-2 water %.1f GL", nat$scope1_gl, nat$scope2_gl))
}
if (length(res)) {
  fwrite(rbindlist(res), file.path(ROOT, out_dirname("operating"), "tab23_monetised_ledger.csv"))
  fwrite(P, file.path(ROOT, out_dirname("operating"), "tab23_ledger_parameters.csv"))
  writeLines(summ, file.path(ROOT, out_dirname("operating"), "tab23_monetised_summary.txt")); cat(summ, sep = "\n")
}
msg("23 done")
