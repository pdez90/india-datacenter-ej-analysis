# =============================================================================
# 31_sensitivity_disparity.R  —  does the receiving-side distribution depend on the allocation choices?
# -----------------------------------------------------------------------------
# Reviewer request (10 Oct 2026). Compares the operating-scope InMAP results of the baseline run with
# the sensitivity runs produced by scripts/run_sensitivity.R (DCEJ_SENS=<name>; 16b -> 16c -> 16d -> 20b
# with suffixed outputs):
#   alloc_gen    emissions placed on a state's plants in proportion to estimated 2017 generation
#                (GPPD estimated_generation_gwh_2017) instead of nameplate capacity;
#   w_flat       operator-class capacity weights 1:1:1:1:1 (load-state tonnes from 15c);
#   w_hyper_only weights 30:3:0.5:1:1 (the scheme that moves the most load between states);
#   unresolved_zero the 54 operating facilities the 24-25 Oct 2026 row review could not confirm carry
#                no load, every other facility unchanged (no recalibration): their contribution alone.
# Writes tab31_sensitivity_disparity.csv (long: run x contrast: ratio, Z, bootstrap sign probability),
# tab31_sensitivity_summary.csv (wide, for SI Table S4.1.3) and tab31_sensitivity_summary.txt.
# Runs that have not been produced yet are listed and skipped.
# =============================================================================
source(file.path(Sys.getenv("DCEJ_ROOT", unset = (function(){a<-commandArgs(FALSE);f<-sub("^--file=","",grep("^--file=",a,value=TRUE));d<-if(length(f)) dirname(normalizePath(f[1])) else getwd();while(!file.exists(file.path(d,"scripts","00_config.R"))&&d!=dirname(d)) d<-dirname(d);d})()), "scripts", "00_config.R"))
assert(DC_SCOPE == "operating", "31 runs on the operating scope")
RUNS <- c(baseline = "", alloc_gen = "alloc_gen", w_flat = "w_flat", w_hyper_only = "w_hyper_only", unresolved_zero = "unresolved_zero")
num <- function(L, pat) as.numeric(sub(pat, "\\1", grep(pat, L, value = TRUE)[1]))
rows <- list(); head <- list()
for (nm in names(RUNS)) {
  sfx <- if (nzchar(RUNS[[nm]])) paste0("_", RUNS[[nm]]) else ""
  fs <- out(sprintf("tab16_inmap_summary%s.txt", sfx)); fd <- out(sprintf("tab20b_inmap_disparity_national%s.csv", sfx))
  if (!file.exists(fs) || !file.exists(fd)) { msg("run %s not available yet (%s / %s)", nm, basename(fs), basename(fd)); next }
  # Every compiled run must have been built from the CORRECTED plant coordinates (21 Oct 2026,
  # data/public/plant_corrections.csv). run_sensitivity.R skips a variant whose 16d output already
  # exists, so a field left over from before the correction would otherwise be compiled silently.
  if (!is_v1) {
    pcf <- file.path(dir_data, "public", "plant_corrections.csv")
    ptf <- out(sprintf("tab16b_emissions_points%s.csv", sfx))
    if (file.exists(pcf) && file.exists(ptf)) {
      pc <- fread(pcf, colClasses = "character"); pt <- fread(ptf)
      col <- c(latitude = "lat", longitude = "lon")      # 16b writes the points table as lon/lat
      for (j in seq_len(nrow(pc))) {
        e <- pc[j]; k <- which(pt$gppd_idnr == e$gppd_idnr); cl <- unname(col[e$field])
        assert(!is.na(cl) && cl %in% names(pt), "plant-correction check: no points column for field %s", e$field)
        assert(length(k) == 1 && abs(pt[[cl]][k] - as.numeric(e$new_value)) < 1e-6,
               "run %s was built before the %s plant-coordinate correction: delete outputs/*%s* and rerun scripts/run_sensitivity.R --only %s",
               nm, e$name, sfx, nm)
      }
    }
  }
  L <- readLines(fs)
  head[[nm]] <- data.table(run = nm,
    pwm_grid = num(L, ".*\\(grid\\): ([0-9.]+) ug.*"), pwm_district = num(L, ".*comparison:\\s+([0-9.]+) ug.*"),
    pop_ge_0_01_million = num(L, ".*>= 0.01 ug/m3 \\(grid cells\\): ([0-9.]+) million.*"),
    deaths = num(L, ".*increment: ([0-9.]+) /yr.*"))
  d <- fread(fd); rows[[nm]] <- d[, .(run = nm, contrast, ratio, Z, boot_sign_prob)]
}
assert(length(rows) >= 1 && "baseline" %in% names(rows), "baseline run missing: run 16d and 20b first")
long <- rbindlist(rows); hd <- rbindlist(head)
fwrite(merge(long, hd, by = "run"), out("tab31_sensitivity_disparity.csv"))
wide <- dcast(long, contrast ~ run, value.var = c("ratio", "boot_sign_prob"))
# order contrasts as in Table S4.1.1 and runs as in RUNS
wide <- wide[match(rows$baseline$contrast, contrast)]
fwrite(wide, out("tab31_sensitivity_summary.csv"))
b <- rows$baseline
missing <- setdiff(names(RUNS), names(rows))
status <- if (length(missing)) {
  sprintf("STATUS: INCOMPLETE - Table S4.1.3 is provisional. Missing run(s): %s. Until they finish, this table shows the baseline only and says nothing about how exposure, mortality or the distributional contrasts respond to the allocation choices. Produce them with: Rscript scripts/run_sensitivity.R", paste(missing, collapse = ", "))
} else {
  "STATUS: complete - all allocation variants compiled."
}
txt <- c(sprintf("Sensitivity of the receiving-side distribution (31), DC_SCOPE=%s, INVENTORY=%s, %s", DC_SCOPE, INVENTORY, format(Sys.time(), "%Y-%m-%d %H:%M")),
         status,
         sprintf("runs available: %s", paste(names(rows), collapse = ", ")),
         sprintf("%-14s pw increment (district) %s | deaths %s", "", paste(sprintf("%s %.4f", hd$run, hd$pwm_district), collapse = " / "), paste(sprintf("%s %.0f", hd$run, hd$deaths), collapse = " / ")))
for (nm in names(rows)[-1]) {
  m <- merge(b, rows[[nm]], by = "contrast", suffixes = c("_b", "_s"))
  m[, sign_same := sign(Z_b) == sign(Z_s)]
  m[, star_b := boot_sign_prob_b < 0.05]; m[, star_s := boot_sign_prob_s < 0.05]
  txt <- c(txt, sprintf("%s: max |ratio change| %.3f (%s); %d of %d contrasts keep their sign; distinguishable at 0.05: baseline %s -> %s",
                        nm, m[, max(abs(ratio_s - ratio_b))], m[which.max(abs(ratio_s - ratio_b)), contrast], m[, sum(sign_same)], nrow(m),
                        paste(m[star_b == TRUE, contrast], collapse = "; "), paste(m[star_s == TRUE, contrast], collapse = "; ")))
}
writeLines(txt, out("tab31_sensitivity_summary.txt")); cat(paste(txt, collapse = "\n"), "\n")
print(wide, digits = 3)
msg("31 done")
