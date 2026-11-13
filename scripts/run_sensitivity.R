# =============================================================================
# run_sensitivity.R  —  the reviewer-requested dispersion sensitivity runs (operating scope)
# -----------------------------------------------------------------------------
#   Rscript scripts/run_sensitivity.R                 # the three runs in the paper (about 6-7 h of InMAP each)
#   Rscript scripts/run_sensitivity.R --only w_flat   # one run
#   Rscript scripts/run_sensitivity.R --compile       # only 31 (after the runs)
# Each run sets DCEJ_SENS=<name> and executes 16b -> 16c -> 16d -> 20b; every output file is suffixed
# with _<name>, so the baseline tables, the shared inmap_work fields and check_pipeline are untouched.
# A run whose 16d output already exists is skipped (delete outputs/tab16_inmap_summary_<name>.txt to redo).
# The Mac is kept awake by 16c (caffeinate). Resume after an interruption by running the same command.
# =============================================================================
args <- commandArgs(TRUE)
root <- (function(){a<-commandArgs(FALSE);f<-sub("^--file=","",grep("^--file=",a,value=TRUE));d<-if(length(f)) dirname(normalizePath(f[1])) else getwd();while(!file.exists(file.path(d,"scripts","00_config.R"))&&d!=dirname(d)) d<-dirname(d);d})()
Sys.setenv(DCEJ_ROOT = root, DC_SCOPE = "operating")
sdir <- file.path(root, "scripts"); rscript <- file.path(R.home("bin"), "Rscript")
run <- function(script, env = character()) {
  cat(sprintf("\n==== %s  [%s]\n", script, paste(env, collapse = " ")))
  st <- system2(rscript, file.path(sdir, script), env = c("DC_SCOPE=operating", env), stdout = "", stderr = "")
  if (st != 0) stop(sprintf("%s failed (%s)", script, paste(env, collapse = " ")))
}
RUNS <- c("w_flat", "w_hyper_only", "unresolved_zero")   # SI Table S4.1.3
if ("--only" %in% args) RUNS <- args[which(args == "--only") + 1]
if (!"--compile" %in% args) {
  run("15c_weight_sensitivity.R")
  first <- TRUE
  for (s in RUNS) {
    done <- file.path(root, "outputs", sprintf("tab16_inmap_summary_%s.txt", s))
    if (file.exists(done)) { cat(sprintf("\n%s already done (%s); skipping\n", s, basename(done))); next }
    env <- paste0("DCEJ_SENS=", s)
    run("16b_build_emissions.R", env)
    run("16c_run_inmap.R", c(env, if (!first) "INMAP_SKIP_PREFLIGHT=1"))
    run("16d_postprocess_inmap.R", env)
    run("20b_inmap_disparity.R", env)
    first <- FALSE
  }
}
run("31_sensitivity_disparity.R")
