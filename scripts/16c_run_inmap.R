# =============================================================================
# 16c_run_inmap.R  —  disperse the plant emissions with Global InMAP (SI S4.1)
# -----------------------------------------------------------------------------
# Model: InMAP v1.9.6 (Tessum et al. 2017), steady state, on the pre-built global
# variable-resolution grid of Thakrar et al. (2022): Zenodo 10.5281/zenodo.6189451,
# EvaluationData_v1.1.0.zip (1.27 GB, CC-BY 4.0; the notes record a "1.27 GB
# EvaluationData zip" in inmap_work). The grid file global_inmap_004x003_v1.1.0.gob
# (nested zip inside it; GridProj longlat, CensusPopColumns TotalPop per the
# authors' sampleConfig.toml)
# already carries the GEOS-Chem-derived chemistry/meteorology and GPW v4.11 2020
# population (TotalPop), so the run needs only the grid and the emissions.
#
# What this script does (all on your Mac; ~6-7.5 h for the full run):
#   1. downloads the InMAP binary for this machine into tools/ (GitHub release);
#   2. downloads the Zenodo zip into inmap_work/ (resumable) and extracts the .gob;
#   3. writes inmap_work/dc_config.toml (static pre-built grid, kg/year emissions);
#   4. PREFLIGHT: a 2-iteration run (a few minutes) and checks that the grid loads,
#      the output variables exist, India's population is ~1.4 bn and the plant
#      emissions landed — so a mistake costs minutes, not a night;
#   5. the full run to convergence -> inmap_work/dc_inmap_output.shp, stamped with
#      the emission scope in inmap_work/dc_inmap_scope.txt (16d refuses a mismatch).
# The Mac is kept awake with `caffeinate -i` while InMAP runs.
#
#   DC_SCOPE=operating Rscript scripts/16b_build_emissions.R
#   Rscript scripts/16c_run_inmap.R                  # preflight + full run
#   INMAP_PREFLIGHT_ONLY=1 Rscript scripts/16c_run_inmap.R
#   INMAP_SKIP_PREFLIGHT=1 Rscript scripts/16c_run_inmap.R
#
# Golden (run 3, v1 inventory, operating, 1 Sep 2026, 6.37 h; the current v2 run gives
# 0.0593 grid (0.0594 district mean) / 1,290 M / 2,047 deaths): population-weighted mean increment
# 0.0585 ug/m3 (grid); 1,234 M people >= 0.01; GEMM deaths 1,976 (checked in 16d).
# If a v1 rerun comes out ~10% HIGHER than 0.0585, the original run most likely
# fed metric tonnes under InMAP's "tons/year" (= short tons) — note it, don't "fix" it.
# =============================================================================
source(file.path(Sys.getenv("DCEJ_ROOT", unset = (function(){a<-commandArgs(FALSE);f<-sub("^--file=","",grep("^--file=",a,value=TRUE));d<-if(length(f)) dirname(normalizePath(f[1])) else getwd();while(!file.exists(file.path(d,"scripts","00_config.R"))&&d!=dirname(d)) d<-dirname(d);d})()), "scripts", "00_config.R"))

ZENODO_URL <- "https://zenodo.org/records/6189451/files/EvaluationData_v1.1.0.zip?download=1"
ZIP        <- file.path(dir_inmap, "EvaluationData_v1.1.0.zip")
GOB_NAME   <- "global_inmap_004x003_v1.1.0.gob"
GOB        <- file.path(dir_inmap, GOB_NAME)
# Sensitivity runs (10 Oct 2026, reviewer request): DCEJ_SENS=<name> keeps every output of this
# script apart from the baseline by suffixing file names with _<name> (see scripts/run_sensitivity.R).
SENS <- Sys.getenv("DCEJ_SENS", ""); SFX <- if (nzchar(SENS)) paste0("_", SENS) else ""
OUT_TAG    <- paste0(INVENTORY, if (DC_SCOPE == "all") "_all" else "", SFX)             # each scope (and sensitivity run) keeps its own field
EMIS       <- file.path(dir_inmap, sprintf("dc_emissions_%s.shp", OUT_TAG))            # written by 16b for this scope
EMIS_STAMP <- file.path(dir_inmap, sprintf("dc_emissions_%s_scope.txt", OUT_TAG))
# InMAP's Go runtime crashed at random (memory faults in its parallel loop) on Apple silicon until
# asynchronous preemption was switched off; set it for every InMAP call (2 Oct 2026).
Sys.setenv(GODEBUG = "asyncpreemptoff=1")
OUT_FULL   <- file.path(dir_inmap, sprintf("dc_inmap_output_%s.shp", OUT_TAG))     # v1 run of 30 Sep 2026 wrote dc_inmap_output.shp
OUT_PRE    <- file.path(dir_inmap, "preflight", "dc_inmap_preflight.shp")
dir.create(dirname(path_inmap_bin), showWarnings = FALSE, recursive = TRUE)
dir.create(dirname(OUT_PRE), showWarnings = FALSE, recursive = TRUE)

fetch <- function(url, dest) {   # curl: follows redirects and resumes a partial download
  st <- system2("curl", c("-L", "--fail", "--retry", "5", "-C", "-", "-o", shQuote(dest), shQuote(url)))
  assert(st == 0 && file.exists(dest), "download failed: %s", url)
}

# ---- 1. binary ---------------------------------------------------------------------------
if (!file.exists(path_inmap_bin)) {
  url <- sprintf("https://github.com/spatialmodel/inmap/releases/download/%s/%s", INMAP_VERSION, basename(path_inmap_bin))
  msg("downloading %s", url); fetch(url, path_inmap_bin)
}
Sys.chmod(path_inmap_bin, "755")
if (Sys.info()[["sysname"]] == "Darwin") suppressWarnings(system2("xattr", c("-d", "com.apple.quarantine", shQuote(path_inmap_bin)), stderr = FALSE))
v <- system2(path_inmap_bin, "version", stdout = TRUE, stderr = TRUE)
msg("InMAP binary: %s | %s", basename(path_inmap_bin), paste(v, collapse = " "))

# ---- 2. grid data --------------------------------------------------------------------------
if (!file.exists(GOB)) {
  if (!file.exists(ZIP) || file.size(ZIP) < 1.2e9) { msg("downloading the Global InMAP data (1.3 GB) from Zenodo ..."); fetch(ZENODO_URL, ZIP) }
  lst <- as.data.table(unzip(ZIP, list = TRUE))
  fwrite(lst, file.path(dir_inmap, "EvaluationData_v1.1.0_contents.csv"))
  # v1.1.0 layout (checked 30 Sep 2026): the .gob sits inside a NESTED zip,
  # EvaluationData_v1.1.0/global_inmap_004x003_v1.1.0.zip (203 MB -> 1.12 GB .gob);
  # README.txt and sampleConfig.toml (the authors' config) are alongside it.
  ev <- file.path(dir_inmap, "evaldata"); dir.create(ev, showWarnings = FALSE)
  lst <- lst[!grepl("^__MACOSX/", Name)]
  inner <- lst[basename(Name) == sub("\\.gob$", ".zip", GOB_NAME), Name]
  assert(length(inner) == 1, "no %s inside the Zenodo zip (see inmap_work/EvaluationData_v1.1.0_contents.csv)", sub("\\.gob$", ".zip", GOB_NAME))
  docs <- lst[basename(Name) %in% c("README.txt", "sampleConfig.toml"), Name]
  unzip(ZIP, files = c(inner, docs), exdir = ev, junkpaths = TRUE)
  unzip(file.path(ev, basename(inner)), files = GOB_NAME, exdir = dir_inmap)
  assert(file.exists(GOB), "extracting %s failed", GOB_NAME)
  msg("grid file ready: %s (%.2f GB)", GOB, file.size(GOB) / 1e9)
}

# ---- 3. config --------------------------------------------------------------------------------
assert(file.exists(EMIS), "run 16b first: %s is missing", EMIS)
ES_LINES <- readLines(EMIS_STAMP)
emis_scope <- ES_LINES[1]
assert(emis_scope == DC_SCOPE, "%s was built for DC_SCOPE=%s, but this run is DC_SCOPE=%s: rerun 16b", basename(EMIS), emis_scope, DC_SCOPE)
emis_inv <- sub("^INVENTORY=", "", grep("^INVENTORY=", ES_LINES, value = TRUE))
EMIS_MD5 <- grep("^EMIS_MD5=", ES_LINES, value = TRUE)
assert(length(EMIS_MD5) == 1 && EMIS_MD5 == paste0("EMIS_MD5=", unname(tools::md5sum(out(sprintf("tab16b_emissions_points%s.csv", SFX))))),
       "%s does not match this scope's tab16b_emissions_points.csv: rerun 16b", basename(EMIS))
assert(length(emis_inv) == 1 && emis_inv == INVENTORY, "inmap_work/dc_emissions.shp was built for inventory %s, but this run is %s: rerun 16b",
       if (length(emis_inv)) emis_inv else "unknown (pre-v2 stamp)", INVENTORY)
write_cfg <- function(path, out_shp, n_iter, logf) {
  q <- function(x) sprintf('"%s"', normalizePath(x, mustWork = FALSE))
  writeLines(c(
    "# written by scripts/16c_run_inmap.R",
    sprintf("VariableGridData = %s", q(GOB)),
    sprintf("InMAPData = %s", q(file.path(dir_inmap, "unused_static_grid.ncf"))),   # not read with a pre-built static grid
    sprintf("EmissionsShapefiles = [%s]", q(EMIS)),
    'EmissionUnits = "kg/year"',
    sprintf("OutputFile = %s", q(out_shp)),
    sprintf("LogFile = %s", q(logf)),
    "OutputAllLayers = false",
    sprintf("NumIterations = %d", n_iter),
    "static = true",
    "creategrid = false",
    "",
    "[OutputVariables]",
    'TotalPop = "TotalPop"',
    'TotalPM25 = "PrimaryPM25 + pNH4 + pSO4 + pNO3 + SOA"',
    'PrimPM25 = "PrimaryPM25"',
    'pSO4 = "pSO4"', 'pNO3 = "pNO3"', 'pNH4 = "pNH4"', 'SOA = "SOA"',
    "",
    "[VarGrid]",
    'GridProj = "+proj=longlat +units=degrees"',
    'CensusPopColumns = ["TotalPop"]',
    'PopGridColumn = "TotalPop"',
    "",
    "[VarGrid.MortalityRateColumns]   # as in the authors' sampleConfig.toml (gob layout)",
    'AllCause = "TotalPop"'), path)
  path
}
run_inmap <- function(cfg) {
  args <- c("run", "steady", sprintf("--config=%s", shQuote(cfg)), "--static=true", "--creategrid=false")
  t0 <- Sys.time()
  st <- if (nzchar(Sys.which("caffeinate"))) system2("caffeinate", c("-i", shQuote(path_inmap_bin), args)) else system2(path_inmap_bin, args)
  msg("InMAP exit status %s after %.2f h", st, as.numeric(difftime(Sys.time(), t0, units = "hours")))
  assert(st == 0, "InMAP failed; see the log named in %s", cfg)
}

# India population and emitted-mass checks on an output file
india <- local({ d <- st_transform(read_districts(), 4326); st_union(st_simplify(st_make_valid(d), dTolerance = 0.02)) })
check_output <- function(shp) {
  o <- st_read(shp, quiet = TRUE)
  need <- c("TotalPop", "TotalPM25", "PrimPM25", "pSO4", "pNO3")
  assert(all(need %in% names(o)), "output lacks %s", paste(setdiff(need, names(o)), collapse = ", "))
  o <- st_transform(o, 4326)
  bb <- st_bbox(c(xmin = 67.5, ymin = 6, xmax = 98, ymax = 37.5), crs = 4326)
  o <- o[lengths(st_intersects(o, st_as_sfc(bb))) > 0, ]
  cen <- suppressWarnings(st_centroid(st_geometry(o)))
  inI <- lengths(st_intersects(cen, india)) > 0
  pop <- sum(o$TotalPop[inI]); pw <- sum(o$TotalPop[inI] * o$TotalPM25[inI]) / pop
  msg("%s: %d cells in the India box; India population %.3f bn; pop-weighted TotalPM25 %.4g ug/m3; max %.3g",
      basename(shp), nrow(o), pop / 1e9, pw, max(o$TotalPM25[inI]))
  assert(pop > 1.2e9 && pop < 1.6e9, "India population %.2f bn is outside 1.2-1.6 bn: wrong CensusPopColumns or grid projection", pop / 1e9)
  assert(max(o$TotalPM25[inI]) > 0, "no PM2.5 increment over India: the emissions did not land in the grid (projection/units?)")
  invisible(list(pop = pop, pwm = pw))
}

# Emission totals InMAP reports after gridding ("Emission totals:" block in its log, ug/s)
# must equal what 16b wrote: emissions falling outside the grid would be silently dropped.
check_emis_log <- function(logf) {
  L <- readLines(logf); i <- grep("Emission totals:", L)[1]
  assert(!is.na(i), "no 'Emission totals' block in %s", logf)
  val <- function(sp) as.numeric(sub(sprintf(".*\\b%s, ([0-9.eE+-]+) .*", sp), "\\1", grep(sprintf(" %s, ", sp), L[i:(i + 12)], value = TRUE)[1]))
  e <- st_drop_geometry(st_read(EMIS, quiet = TRUE)); k <- 1e9 / (3600 * 8760)     # kg/yr -> ug/s, as InMAP
  want <- c(PM2_5 = sum(e$PM2_5) * k, gS = sum(e$SOx) * k * 32.06 / 64.066, gNO = sum(e$NOx) * k * 14.007 / 46.006)
  got  <- c(PM2_5 = val("PM2_5"), gS = val("gS"), gNO = val("gNO"))
  msg("InMAP emission totals / 16b totals: PM2.5 %.4f, S %.4f, N %.4f", got[1] / want[1], got[2] / want[2], got[3] / want[3])
  assert(all(abs(got / want - 1) < 0.03), "InMAP gridded emissions differ from 16b by more than 3%% (plants outside the grid, or wrong units)")
}

# ---- 4. preflight ----------------------------------------------------------------------------
if (Sys.getenv("INMAP_SKIP_PREFLIGHT") != "1") {
  msg("PREFLIGHT: 2 iterations (loading the global grid takes several minutes) ...")
  lp <- file.path(dir_logs, sprintf("inmap_preflight_%s_%s.log", OUT_TAG, format(Sys.Date(), "%Y%m%d")))
  run_inmap(write_cfg(file.path(dir_inmap, "dc_config_preflight.toml"), OUT_PRE, 2L, lp))
  check_emis_log(lp)
  check_output(OUT_PRE)
  msg("preflight passed")
  if (Sys.getenv("INMAP_PREFLIGHT_ONLY") == "1") quit(save = "no")
}

# ---- 5. full run -----------------------------------------------------------------------------
msg("FULL RUN (DC_SCOPE=%s): expect 6-7.5 h; the Mac is kept awake. Log: outputs/run_logs/inmap_rerun_%s.log", DC_SCOPE, format(Sys.Date(), "%Y%m%d"))
N_ITER <- as.integer(Sys.getenv("INMAP_NITER", "0"))   # 0 = run to convergence (default); >0 = stop early (units test only)
if (N_ITER > 0) msg("INMAP_NITER=%d: the run stops early and is NOT converged (units test only)", N_ITER)
run_inmap(write_cfg(file.path(dir_inmap, "dc_config.toml"), OUT_FULL, N_ITER,
                    file.path(dir_logs, sprintf("inmap_rerun_%s_%s.log", OUT_TAG, format(Sys.Date(), "%Y%m%d")))))
r <- check_output(OUT_FULL)
stamp_lines <- c(DC_SCOPE, format(Sys.time()), sprintf("InMAP %s", INMAP_VERSION), basename(GOB), paste0("INVENTORY=", INVENTORY), EMIS_MD5, if (nzchar(SENS)) paste0("SENS=", SENS), if (N_ITER > 0) sprintf("NOT CONVERGED: NumIterations=%d", N_ITER))
if (!nzchar(SENS)) writeLines(stamp_lines, file.path(dir_inmap, "dc_inmap_scope.txt"))
writeLines(stamp_lines, file.path(dir_inmap, sprintf("dc_inmap_scope_%s.txt", OUT_TAG)))   # per-field stamp read by 16d
if (DC_SCOPE == "operating")
  msg("population-weighted increment %.4f ug/m3 over India cells (published 0.0585 grid-weighted; 16d computes the exact figure)", r$pwm)
