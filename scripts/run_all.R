# =============================================================================
# run_all.R  —  runs the whole pipeline in order, both scopes where relevant
# -----------------------------------------------------------------------------
#   Rscript scripts/run_all.R --skip-gee                      # everything (incl. ~12 h of InMAP)
#   Rscript scripts/run_all.R --skip-gee --skip-inmap         # everything except the two InMAP runs (16c);
#                                                             #   16d onwards then reuse the existing fields,
#                                                             #   which 16d accepts only if they were made from
#                                                             #   the current emissions (content hash)
#   Rscript scripts/run_all.R --from 16d                      # start at the first step whose name begins "16d"
#   Rscript scripts/run_all.R --from 16d --until 23           # ... and stop after the step beginning "23"
#   Rscript scripts/run_all.R --skip 04,05                    # skip steps whose raw inputs are not on this machine
# Each step runs in a fresh process (so a failed invariant stops the run); R and Python steps alike.
# A one-line record per step (exit status, seconds) goes to outputs/run_logs/run_all_<date>.log, and
# the full console output to outputs/run_logs/run_all_<date>_console.log when launched as in README.
# Not run here (network, slow, or interactive): 03 with --skip-gee, 25_archive_sources.py (Wayback),
# 26/27 (link checks and snapshots). Their outputs are inputs to 24 and are kept in data/public.
# =============================================================================
args <- commandArgs(TRUE)
arg_val <- function(k) if (k %in% args) args[which(args == k) + 1] else ""
from <- arg_val("--from"); until <- arg_val("--until")
skip_gee <- "--skip-gee" %in% args
skip_inmap <- "--skip-inmap" %in% args
skip <- if (nzchar(arg_val("--skip"))) strsplit(arg_val("--skip"), ",")[[1]] else character(0)

a <- commandArgs(FALSE); f <- sub("^--file=", "", grep("^--file=", a, value = TRUE))
sdir <- if (length(f)) dirname(normalizePath(f[1])) else file.path(getwd(), "scripts")
root <- dirname(sdir)
logf <- file.path(root, "outputs", "run_logs", sprintf("run_all_%s.log", format(Sys.Date())))
dir.create(dirname(logf), recursive = TRUE, showWarnings = FALSE)
# Python for the .py steps: DCEJ_PYTHON if set, else the first interpreter on this machine that can
# import numpy and pandas (a Mac can carry several Pythons, and a `python3` whose numpy is an Intel build
# fails on Apple silicon; 4 Oct 2026). Checked once here, before any step runs.
py_ok <- function(p) nzchar(p) && file.exists(p) &&
  identical(tryCatch(system2(p, c("-c", shQuote("import numpy, pandas; print('ok')")), stdout = TRUE, stderr = FALSE), error = function(e) ""), "ok")
cands <- c(Sys.getenv("DCEJ_PYTHON"), Sys.which("python3"), Sys.which("python"),
           "/opt/homebrew/bin/python3", "/usr/local/bin/python3", "/usr/bin/python3",
           "/Library/Frameworks/Python.framework/Versions/3.12/bin/python3", "/Library/Frameworks/Python.framework/Versions/3.11/bin/python3",
           "/Library/Frameworks/Python.framework/Versions/3.10/bin/python3",
           path.expand("~/.virtualenvs/rgee/bin/python"), "/opt/miniconda3/bin/python3", "/opt/anaconda3/bin/python3")
cands <- unique(cands[nzchar(cands)])
python <- ""; for (p in cands) if (py_ok(p)) { python <- p; break }
if (!nzchar(python)) stop("no Python with numpy and pandas found (tried: ", paste(cands, collapse = ", "), "); set DCEJ_PYTHON=/path/to/python3")
cat(sprintf("Python for .py steps: %s\n", python))

if (Sys.getenv("DCEJ_SOFT_ASSERTS") == "1") stop("run_all.R refuses to run with DCEJ_SOFT_ASSERTS=1: every golden would be downgraded to a message; unset it")
O <- "operating"; B <- c("operating", "all")
steps <- list(
  # script                       scopes  note
  list("00a_districts.R",         O),     # district layer: Census 2011 boundaries + NFHS identifiers (v2)
  list("00b_inventory_v2.R",      O),     # revised (v2) inventory from v1 + sourced corrections and additions
  list("01_inventory.R",          O),     # writes both scopes itself
  list("03_gee_extract.R",        O),     # Earth Engine (rgee); skipped with --skip-gee (cached CSVs in data/gee)
  list("04_env_local.R",          O),     # also downloads/caches the WRI power-plant database used by 16b
  list("04b_env_external.R",      O),     # LGHAP PM2.5, Wang et al. ozone, Meta RWI (downloads ~22 GB on first run)
  list("05_nfhs_district.R",      O),
  list("05e_water.R",             O),     # both scopes itself
  list("06_analysis_frame.R",     O),
  list("07_siting.R",             B),
  list("07c_grid_shadow.R",       B),
  list("10_permutation.R",        O),
  list("20a_disparity.R",         B),
  list("15_attribution.R",        B),
  list("15a_mef_kilthub.R",       O),
  list("15b_marginal_mef.R",      B),
  list("15c_weight_sensitivity.R", B),
  list("13_burden.R",             B),
  list("14_policy_table.R",       B),
  list("16b_build_emissions.R",   B),     # per-scope emissions shapefile + content hash
  list("16c_run_inmap.R",         B),     # Global InMAP, ~6 h per scope; skipped with --skip-inmap
  list("16d_postprocess_inmap.R", B),
  list("16e_group_exposure.R",    B),
  list("16f_inmap_figures.R",     B),
  list("20b_inmap_disparity.R",   B),
  list("31_sensitivity_disparity.R", O),  # Table S4.1.3: compiles the run_sensitivity.R fields that exist (baseline only if none)
  list("21_benefit_burden.R",     B),
  list("23_monetised_ledger.R",   O),     # both scopes itself
  list("32_si_descriptives.R",    O),     # SI numbers derived from outputs (coal-district profile, thermal-mix water, unweighted MEF, ...)
  list("28_esmi_outages.py",      O),     # Prayas ESMI district interruption hours (data/esmi)
  list("29_backup_diesel.R",      B),     # backup-diesel runtime and emissions (SI S5)
  list("24_public_inventory.R",   O),     # public sourced facility table (+ copy into the app); 17 E and 19 Fig 1 read it
  list("17_siting_robustness.R",  O),     # SI S10 and the high-confidence sensitivity
  list("19_figures.R",            O),     # SI and main-text maps and figures
  list("19b_figure1.py",          O),     # Figure 1 (inventory and capacity flow), matplotlib, counts read and asserted
  list("19c_figure_s83.py",        O),     # Figure S8.3 (facility-to-receiving-communities schematic)
  list("30_app_download_tables.R", O),    # documented download tables for the app (power interruptions -> diesel; emission factors)
  list("22_build_shiny_data.py",  O),     # companion app data layer (bundles 30's tables into the documented zip)
  list("22b_inmap_grid.R",        O),     # InMAP grid cut into district pieces for the app's Air-quality model tab
  list("check_pipeline.R",        O)      # registry of invariants and published values
)
started <- from == ""
for (s in steps) {
  nm <- s[[1]]
  if (!started && startsWith(nm, from)) started <- TRUE
  if (!started) next
  done_after <- nzchar(until) && startsWith(nm, until)
  run_it <- !(skip_gee && nm == "03_gee_extract.R") && !(skip_inmap && startsWith(nm, "16c_")) &&
            !any(startsWith(nm, paste0(skip, "_")))
  if (run_it && !file.exists(file.path(sdir, nm))) stop(sprintf("step %s: script not found in %s", nm, sdir))
  if (run_it) for (sc in s[[2]]) {
    cat(sprintf("\n==== %s  [DC_SCOPE=%s]\n", nm, sc))
    t0 <- Sys.time()
    py <- grepl("\\.py$", nm)
    tries <- if (startsWith(nm, "16c_")) 3 else 1          # InMAP: retry a random runtime crash (preflight is skipped on retries)
    for (k in seq_len(tries)) {
      env <- c(paste0("DC_SCOPE=", sc), if (k > 1) "INMAP_SKIP_PREFLIGHT=1")
      status <- if (py) system2(python, file.path(sdir, nm), env = env, stdout = "", stderr = "")
                else system2(file.path(R.home("bin"), "Rscript"), file.path(sdir, nm), env = env, stdout = "", stderr = "")
      if (status == 0) break
      if (k < tries) cat(sprintf("%s exited %s; retrying (%d of %d)\n", nm, status, k + 1, tries))
    }
    cat(sprintf("%s %s %s exit=%s %.1fs\n", format(Sys.time()), nm, sc, status, as.numeric(difftime(Sys.time(), t0, units = "secs"))),
        file = logf, append = TRUE)
    if (status != 0) stop(sprintf("%s (DC_SCOPE=%s) failed - see messages above", nm, sc))
    if (py) break                                            # Python steps handle both scopes themselves
  }
  if (done_after) break
}
cat("\nAll requested steps completed.\n")
