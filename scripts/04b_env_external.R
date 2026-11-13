# =============================================================================
# 04b_env_external.R  —  district layers rebuilt from three large external datasets
# -----------------------------------------------------------------------------
# Written 3 Oct 2026: these layers were frozen (or missing) after the 29 Sep loss.
#   pm25_lghap   LGHAP v2 monthly mean 1-km gap-free PM2.5 (Bai et al. 2024, ESSD 16, 2425),
#                Zenodo 10.5281/zenodo.14280024 (2000-2021; CC-BY 4.0). Mean of the monthly grids
#                for LGHAP_YEARS (2017-2021: the monthly release ends in 2021, so the paper's
#                "2017-2022" becomes 2017-2021), exact area-weighted district mean.
#   o3_annual    Wang et al. (2025, Sci Data 12, 1710) global land daily 0.1-degree MDA8 surface ozone,
#   o3_summer    Zenodo 10.5281/zenodo.16683952 (2013-2022; CC-BY 4.0; ug/m3). o3_annual = mean of
#                all days 2013-2022; o3_summer = mean of June-August days 2013-2022 (the frozen layer's
#                definition, "summer (JJA)" in 10_permutation). Exact area-weighted district means.
#   rwi_mean     Meta Relative Wealth Index (Chi et al. 2022), India tile file from the Humanitarian
#                Data Exchange (https://data.humdata.org/dataset/relative-wealth-index, file
#                ind_relative_wealth_index.csv; CC-BY-NC 4.0). District mean of the 2.4 km tiles whose
#                centre falls in the district.
# File layout checked 3 Oct 2026 on the downloaded files: both store the grid as VAR(lon, lat), i.e.
# transposed relative to what GDAL/terra assume, and Wang's latitude runs 90 -> -90 with both poles
# included (1800 x 3600 nodes). So the grids are read with ncdf4 by dimension NAME, India subset only,
# never through terra::rast(). LGHAP: PM25 int16, fill 32767, 0.01 deg, lat -58..62. Wang: O3 float,
# ug/m3, NaN over ocean. Means are sum/count per cell (a missing month or day is skipped, not zeroed).
# Zips are unpacked one member at a time (a year of ozone is 9.5 GB unpacked).
# Folders: LGHAP zips in DCEJ_LGHAP_DIR, else DCEJ_EXT_DIR/LGHAP, else ~/Downloads/LGHAP; ozone zips in
# DCEJ_O3_DIR, else DCEJ_EXT_DIR/ozone, else ~/Downloads/ozone. Missing files are downloaded into
# the first of these.
# Downloads go to DCEJ_EXT_DIR (default data/external inside the project), resumable with curl, and
# their SHA-256 sums are recorded in data/raw/external_manifest.csv. Zenodo cannot be reached from
# every network; if a download fails, fetch the file in a browser into the same folder and rerun.
# The Meta RWI file must be downloaded by hand (HDX needs a browser) into DCEJ_EXT_DIR/rwi/.
# Each layer is checked against the frozen 1 Sep 2026 value where one exists (o3_summer, rwi_mean)
# and the comparison is printed; 06 then uses these rebuilt layers in place of the frozen ones.
# Output: data/derived/env_external_district.csv (+ cached India rasters in data/derived/)
#   Rscript scripts/04b_env_external.R                 # all three
#   DCEJ_EXT_ONLY=lghap Rscript scripts/04b_env_external.R
# =============================================================================
source(file.path(Sys.getenv("DCEJ_ROOT", unset = (function(){a<-commandArgs(FALSE);f<-sub("^--file=","",grep("^--file=",a,value=TRUE));d<-if(length(f)) dirname(normalizePath(f[1])) else getwd();while(!file.exists(file.path(d,"scripts","00_config.R"))&&d!=dirname(d)) d<-dirname(d);d})()), "scripts", "00_config.R"))
if (!requireNamespace("ncdf4", quietly = TRUE)) install.packages("ncdf4", repos = "https://cloud.r-project.org")
suppressPackageStartupMessages({ library(terra); library(exactextractr); library(ncdf4) })

EXT  <- path.expand(Sys.getenv("DCEJ_EXT_DIR", file.path(dir_data, "external")))   # data/external/{LGHAP,ozone,rwi}
ONLY <- Sys.getenv("DCEJ_EXT_ONLY", "")
INDIA <- terra::ext(67.9, 98.1, 5.9, 37.6)
LGHAP_YEARS <- 2017:2021
O3_YEARS    <- 2013:2022
dir.create(EXT, showWarnings = FALSE, recursive = TRUE)
dir_for <- function(envvar, sub, alt) {        # first existing folder; else the download folder
  c0 <- c(Sys.getenv(envvar), file.path(EXT, sub), path.expand(alt)); c0 <- c0[nzchar(c0)]
  hit <- c0[dir.exists(c0)]; if (length(hit)) hit[1] else file.path(EXT, sub) }
LGHAP_DIR <- dir_for("DCEJ_LGHAP_DIR", "LGHAP", "~/Downloads/LGHAP")
O3_DIR    <- dir_for("DCEJ_O3_DIR", "ozone", "~/Downloads/ozone")
manifest_f <- file.path(dir_data, "raw", "external_manifest.csv"); dir.create(dirname(manifest_f), showWarnings = FALSE)

fetch <- function(url, dest, min_bytes) {
  if (file.exists(dest) && file.size(dest) >= min_bytes) return(invisible(dest))
  dir.create(dirname(dest), showWarnings = FALSE, recursive = TRUE)
  msg("downloading %s", basename(dest))
  st <- system2("curl", c("-L", "--fail", "--retry", "5", "-C", "-", "-o", shQuote(dest), shQuote(url)))
  assert(st == 0 && file.exists(dest) && file.size(dest) >= min_bytes,
         "download failed: %s\n  fetch it in a browser into %s and rerun", url, dirname(dest))
  invisible(dest)
}
record <- function(f, source) {
  if (file.exists(manifest_f)) { m0 <- fread(manifest_f, colClasses = "character"); if (any(m0$file == basename(f) & as.numeric(m0$bytes) == file.size(f))) return(invisible()) }
  sha <- system2("shasum", c("-a", "256", shQuote(f)), stdout = TRUE)
  row <- data.table(file = basename(f), bytes = file.size(f), sha256 = sub(" .*", "", sha[1]), source = source, recorded = format(Sys.Date()))
  m <- if (file.exists(manifest_f)) fread(manifest_f, colClasses = "character")[file != row$file] else NULL
  fwrite(rbind(m, row), manifest_f)
}
zip_members <- function(zipf, pattern) { m <- system2("unzip", c("-Z1", shQuote(zipf)), stdout = TRUE); sort(grep(pattern, m, value = TRUE)) }
unzip_one <- function(zipf, member, td) {      # extract one member (junk paths) and return its path
  # Some members of the Wang et al. zips carry a zero CRC in their local header (the central-directory
  # CRC is right and the data decompress fully), so unzip exits 2 with "bad CRC ... (should be 00000000)".
  # Accept status 0 or 2 when the file came out complete; the NetCDF header check (nc_ok) then decides.
  st <- system2("unzip", c("-q", "-o", "-j", shQuote(zipf), shQuote(member), "-d", shQuote(td)), stderr = FALSE)
  f <- file.path(td, basename(member))
  assert(st %in% c(0L, 2L) && file.exists(f) && file.size(f) > 0, "unzip failed (status %s): %s :: %s", st, zipf, member)
  f }
# Read VAR over the India box by dimension name, whatever the storage order, as a north-up SpatRaster.
read_nc_india <- function(f, var, box = INDIA) {
  nc <- nc_open(f); on.exit(nc_close(nc))
  v <- nc$var[[var]]; assert(!is.null(v), "%s: no variable %s", f, var)
  dn <- vapply(v$dim, function(d) d$name, ""); ix <- grep("^lon", dn); iy <- grep("^lat", dn)
  assert(length(dn) == 2 && length(ix) == 1 && length(iy) == 1, "%s: unexpected dimensions %s", f, paste(dn, collapse = ","))
  x <- v$dim[[ix]]$vals; y <- v$dim[[iy]]$vals
  assert(all(is.finite(x)) && all(is.finite(y)) && length(x) > 100 && length(y) > 100 && min(x) < box[1] && max(x) > box[2] && min(y) < box[3] && max(y) > box[4],
         "%s: coordinate arrays are not a valid global grid (corrupt file)", basename(f))
  xs <- which(x >= box[1] & x <= box[2]); ys <- which(y >= box[3] & y <= box[4])
  st <- integer(2); ct <- integer(2); st[ix] <- min(xs); ct[ix] <- length(xs); st[iy] <- min(ys); ct[iy] <- length(ys)
  a <- ncvar_get(nc, v, start = st, count = ct, collapse_degen = FALSE)
  m <- if (ix == 1) t(a) else a                 # m[y, x]
  xv <- x[st[ix] + seq_len(ct[ix]) - 1]; yv <- y[st[iy] + seq_len(ct[iy]) - 1]
  if (yv[1] < yv[length(yv)]) { m <- m[nrow(m):1, , drop = FALSE]; yv <- rev(yv) }   # north at the top
  if (xv[1] > xv[length(xv)]) { m <- m[, ncol(m):1, drop = FALSE]; xv <- rev(xv) }
  dx <- (max(xv) - min(xv)) / (length(xv) - 1); dy <- (max(yv) - min(yv)) / (length(yv) - 1)
  m[is.nan(m)] <- NA
  rast(m, extent = terra::ext(min(xv) - dx / 2, max(xv) + dx / 2, min(yv) - dy / 2, max(yv) + dy / 2), crs = "EPSG:4326")
}
coverage_check <- function(r, label) {          # a transposed or shifted grid leaves districts empty
  v <- exact_extract(r, d4, "mean", progress = FALSE)
  e <- !is.finite(v); msg("%s: %d of %d districts without data%s", label, sum(e), length(v), if (any(e)) paste0(" (", paste(d4$dist_name[e], collapse = ", "), ")") else "")
  # islands and districts smaller than a 0.1-degree ocean-masked cell can be empty; a transposed or shifted grid empties dozens
  assert(sum(e) <= 25, "%s: %d districts without data - check the grid orientation", label, sum(e))
  # Narrow coastal districts (Mumbai, Daman, Diu, Mahe) can lie entirely in ocean-masked cells of a
  # 0.1-degree land-only grid: they take the mean of the valid cells within 15 km of their boundary.
  # Islands with no valid cell within 15 km (Lakshadweep) stay NA.
  if (any(e)) {
    buf <- st_transform(st_buffer(st_transform(d4[e, ], 32644), 15000), 4326)
    vb <- exact_extract(r, buf, "mean", progress = FALSE); ok <- is.finite(vb)
    v[which(e)[ok]] <- vb[ok]
    msg("%s: filled from cells within 15 km: %s%s", label, if (any(ok)) paste(d4$dist_name[e][ok], collapse = ", ") else "none",
        if (any(!ok)) paste0("; still NA: ", paste(d4$dist_name[e][!ok], collapse = ", ")) else "")
  }
  v }
unzip_tmp <- function(zipf) { td <- file.path(tempdir(), sub("\\.zip$", "", basename(zipf))); dir.create(td, showWarnings = FALSE)
  st <- system2("unzip", c("-q", "-o", shQuote(zipf), "-d", shQuote(td))); assert(st == 0, "unzip failed: %s", zipf); td }
pick_layer <- function(r, pattern) {          # first layer whose name matches, else the first layer
  k <- grep(pattern, names(r), ignore.case = TRUE); r[[if (length(k)) k[1] else 1]] }

d4 <- st_transform(read_districts(), 4326)
res <- data.table(zone_uid = d4$zone_uid)
golden_f <- file.path(dir_data, "golden", "districts.geojson")   # authors' frozen reference layer (not released)
gold <- if (file.exists(golden_f)) as.data.table(st_drop_geometry(st_read(golden_f, quiet = TRUE))) else NULL
compare <- function(new, var) {      # informational: the frozen layer was computed on the warped NFHS polygons
  if (is.null(gold)) return(invisible(NULL))   # reference layer not available: skip the comparison
  g <- merge(res[, .(zone_uid, a = get(new))], gold[, .(zone_uid, b = get(var))], by = "zone_uid")[is.finite(a) & is.finite(b)]
  msg("%s vs frozen %s: n %d, r %.4f, median ratio %.4f, max |diff| %.3f", new, var, nrow(g), cor(g$a, g$b), median(g$a / g$b), max(abs(g$a - g$b)))
}

# ---- LGHAP PM2.5 ----------------------------------------------------------------------------
if (ONLY %in% c("", "lghap")) {
  cache <- file.path(dir_derived, sprintf("lghap_pm25_%d_%d_mean.tif", min(LGHAP_YEARS), max(LGHAP_YEARS)))
  if (!file.exists(cache)) {
    sm <- NULL; nn <- NULL; n <- 0
    for (y in LGHAP_YEARS) {
      zf <- file.path(LGHAP_DIR, sprintf("LGHAP.Global_PM25.M001.Y%d.zip", y))
      fetch(sprintf("https://zenodo.org/records/14280024/files/LGHAP.Global_PM25.M001.Y%d.zip?download=1", y), zf, 1e8)
      record(zf, "LGHAP v2 monthly PM2.5, Zenodo 10.5281/zenodo.14280024")
      mem <- zip_members(zf, "\\.nc4?$"); assert(length(mem) == 12, "%s holds %d monthly files, expected 12", zf, length(mem))
      td <- file.path(tempdir(), "lghap"); dir.create(td, showWarnings = FALSE)
      for (mb in mem) {
        f <- unzip_one(zf, mb, td); r <- read_nc_india(f, "PM25"); unlink(f)
        ok <- !is.na(r); v <- ifel(ok, r, 0)
        if (is.null(sm)) { sm <- v; nn <- ok } else { sm <- sm + v; nn <- nn + ok }
        n <- n + 1
      }
      msg("LGHAP %d: %d months added", y, length(mem))
    }
    assert(n == 12 * length(LGHAP_YEARS), "LGHAP: %d monthly grids, expected %d", n, 12 * length(LGHAP_YEARS))
    writeRaster(sm / nn, cache, overwrite = TRUE)
  }
  lg <- rast(cache)
  res[, pm25_lghap := coverage_check(lg, "LGHAP PM2.5")]
  msg("LGHAP PM2.5 district means: median %.1f, Delhi-area max %.1f ug/m3", median(res$pm25_lghap, na.rm = TRUE), max(res$pm25_lghap, na.rm = TRUE))
  fr0 <- fread(file.path(dir_derived, "env_local_district.csv"))[, .(zone_uid, pm25_acag_local)]   # 04 output (same district layer; 06 runs later)
  k <- merge(res[, .(zone_uid, pm25_lghap)], fr0, by = "zone_uid")[is.finite(pm25_lghap) & is.finite(pm25_acag_local)]
  msg("LGHAP vs ACAG district means: r %.3f, median ratio %.3f", cor(k$pm25_lghap, k$pm25_acag_local), median(k$pm25_lghap / k$pm25_acag_local))
  assert(cor(k$pm25_lghap, k$pm25_acag_local) > 0.8, "LGHAP and ACAG disagree (r < 0.8): wrong variable or scale factor in the LGHAP files")
  assert(between(median(k$pm25_lghap / k$pm25_acag_local), 0.5, 2) && max(res$pm25_lghap, na.rm = TRUE) < 300, "LGHAP scale looks wrong (median ratio to ACAG outside 0.5-2 or max >= 300 ug/m3): scale_factor / _FillValue?")
}

# ---- Wang et al. MDA8 ozone ---------------------------------------------------------------------
if (ONLY %in% c("", "ozone")) {
  ca <- file.path(dir_derived, "wang_o3_mda8_2013_2022_annual.tif"); cs <- file.path(dir_derived, "wang_o3_mda8_2013_2022_jja.tif")
  if (!file.exists(ca) || !file.exists(cs)) {
    sa <- NULL; na_ <- NULL; ss <- NULL; ns <- NULL; nd <- 0; bad_days <- character(0)
    # Two members of 2015.zip (ozone-20150608.nc, ozone-20150609.nc) are zero-filled in the Zenodo
    # upload (checked 3 Oct 2026: every other daily file 2013-2022 has a valid NetCDF header). An
    # unreadable day is skipped and listed; the means are sum/count so a skipped day is simply absent.
    nc_ok <- function(f) { con <- file(f, "rb"); on.exit(close(con)); identical(readBin(con, "raw", 4), as.raw(c(0x43, 0x44, 0x46, 0x01))) ||
                           identical(readBin(f, "raw", 8)[1:4], as.raw(c(0x89, 0x48, 0x44, 0x46))) }
    for (y in O3_YEARS) {
      zf <- file.path(O3_DIR, sprintf("%d.zip", y))
      fetch(sprintf("https://zenodo.org/records/16683952/files/%d.zip?download=1", y), zf, 1e9)
      record(zf, "Wang et al. 2025 daily MDA8 ozone, Zenodo 10.5281/zenodo.16683952")
      mem <- zip_members(zf, "\\d{8}\\.nc$")
      ndays <- as.integer(as.Date(sprintf("%d-12-31", y)) - as.Date(sprintf("%d-01-01", y))) + 1
      assert(length(mem) == ndays, "%s holds %d daily files, expected %d", zf, length(mem), ndays)
      td <- file.path(tempdir(), "o3"); dir.create(td, showWarnings = FALSE)
      for (mb in mem) {
        day <- as.Date(sub(".*(\\d{8})\\.nc$", "\\1", basename(mb)), "%Y%m%d")
        assert(format(day, "%Y") == as.character(y), "%s: %s is not a %d day", zf, mb, y)
        f <- unzip_one(zf, mb, td)
        r <- if (nc_ok(f)) tryCatch(read_nc_india(f, "O3"), error = function(e) { msg("  %s unreadable: %s", basename(mb), conditionMessage(e)); NULL }) else NULL
        unlink(f)
        if (is.null(r)) { bad_days <- c(bad_days, basename(mb)); next }
        ok <- !is.na(r); v <- ifel(ok, r, 0)
        if (is.null(sa)) { sa <- v; na_ <- ok; ss <- v * 0; ns <- ok * 0 } else { sa <- sa + v; na_ <- na_ + ok }
        if (format(day, "%m") %in% c("06", "07", "08")) { ss <- ss + v; ns <- ns + ok }
      }
      nd <- nd + length(mem); msg("ozone %d: %d days", y, length(mem))
    }
    assert(length(bad_days) <= 5, "ozone: %d unreadable daily files (expected the two zero-filled 2015 days and the corrupt 2017-09-17 file): %s", length(bad_days), paste(bad_days, collapse = ", "))
    msg("ozone: %d days in all, %d unreadable and skipped%s", nd, length(bad_days), if (length(bad_days)) paste0(" (", paste(bad_days, collapse = ", "), ")") else "")
    writeLines(c(sprintf("Wang et al. 2025 MDA8 ozone, %d-%d: %d daily files, %d unreadable (zero-filled at source) and skipped:", min(O3_YEARS), max(O3_YEARS), nd, length(bad_days)), bad_days),
               file.path(dir_data, "raw", "ozone_skipped_days.txt"))
    writeRaster(sa / na_, ca, overwrite = TRUE); writeRaster(ss / ns, cs, overwrite = TRUE)
  }
  res[, o3_annual := coverage_check(rast(ca), "ozone annual MDA8")]
  res[, o3_summer := coverage_check(rast(cs), "ozone JJA MDA8")]
  msg("ozone district means (ug/m3): annual median %.1f [%.1f-%.1f], JJA median %.1f", median(res$o3_annual, na.rm = TRUE),
      min(res$o3_annual, na.rm = TRUE), max(res$o3_annual, na.rm = TRUE), median(res$o3_summer, na.rm = TRUE))
  compare("o3_summer", "o3_summer")
}

# ---- Meta Relative Wealth Index ----------------------------------------------------------------
if (ONLY %in% c("", "rwi")) {
  rf <- list.files(file.path(EXT, "rwi"), pattern = "^ind.*relative_wealth_index\\.csv$", full.names = TRUE, ignore.case = TRUE)
  if (!length(rf)) {
    msg("RWI: put ind_relative_wealth_index.csv (https://data.humdata.org/dataset/relative-wealth-index) in %s; skipped", file.path(EXT, "rwi"))
  } else {
    record(rf[1], "Meta Relative Wealth Index, India (HDX)")
    t <- fread(rf[1]); setnames(t, tolower(names(t)))
    pts <- st_as_sf(t[, .(longitude, latitude, rwi)], coords = c("longitude", "latitude"), crs = 4326)
    j <- st_join(pts, d4[, "zone_uid"], join = st_within)
    rw <- as.data.table(st_drop_geometry(j))[!is.na(zone_uid), .(rwi_mean = mean(rwi), rwi_tiles = .N), by = zone_uid]
    res <- merge(res, rw, by = "zone_uid", all.x = TRUE)
    compare("rwi_mean", "rwi_mean")
  }
}

out_f <- file.path(dir_derived, "env_external_district.csv")
if (file.exists(out_f) && nzchar(ONLY)) {          # partial run: keep the other layers
  old <- fread(out_f); keep <- setdiff(names(old), setdiff(names(res), "zone_uid"))
  res <- merge(old[, ..keep], res, by = "zone_uid", all = TRUE)
}
fwrite(res, out_f)
msg("wrote %s: %s", out_f, paste(setdiff(names(res), "zone_uid"), collapse = ", "))
