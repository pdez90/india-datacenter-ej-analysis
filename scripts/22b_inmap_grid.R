# =============================================================================
# 22b_inmap_grid.R  —  the InMAP grid behind the app's Air-quality model tab (operating scope)
# Reads inmap_work/dc_inmap_output_<INVENTORY>.shp (16c) and the district layer, cuts every grid cell at
# the district boundaries exactly as 16d does, and writes india-datacentre-ej/data/inmap_grid.rds:
#   cells  : one row per cell touching a district (lon/lat box, gridded population, TotalPM25 and its
#            species PrimPM25, pSO4, pNO3, pNH4, SOA)
#   pieces : one row per cell x district piece (share of the cell's area inside the district)
# Checks that the pieces reproduce every district increment of tab16_inmap_district.csv (16d).
# Added 19 Oct 2026 for the app's step-by-step air-quality tab.
# =============================================================================
source(file.path(Sys.getenv("DCEJ_ROOT", unset = (function(){a<-commandArgs(FALSE);f<-sub("^--file=","",grep("^--file=",a,value=TRUE));d<-if(length(f)) dirname(normalizePath(f[1])) else getwd();while(!file.exists(file.path(d,"scripts","00_config.R"))&&d!=dirname(d)) d<-dirname(d);d})()), "scripts", "00_config.R"))
assert(DC_SCOPE == "operating", "22b runs for the operating scope")
dist <- read_districts()
o <- st_read(file.path(dir_inmap, sprintf("dc_inmap_output_%s.shp", INVENTORY)), quiet = TRUE)
dist <- st_transform(dist, st_crs(o))
o <- o[lengths(st_intersects(o, st_as_sfc(st_bbox(dist)))) > 0, ]
o$cell <- seq_len(nrow(o)); o$cell_area <- as.numeric(st_area(o))
pc <- suppressWarnings(st_intersection(o, st_make_valid(dist[, c("zone_uid")])))
pc$frac <- as.numeric(st_area(pc)) / pc$cell_area
P <- as.data.table(st_drop_geometry(pc))[, .(zone_uid, cell, frac = pmin(frac, 1))]
cells <- o[o$cell %in% P$cell, ]
assert(all(vapply(st_geometry(cells), function(g) nrow(st_coordinates(g)) == 5, TRUE)), "InMAP cells must be rectangles")
bb <- t(vapply(st_geometry(cells), function(g) as.numeric(st_bbox(g)), numeric(4)))
C <- data.table(cell = cells$cell, xmin = bb[, 1], ymin = bb[, 2], xmax = bb[, 3], ymax = bb[, 4], pop = cells$TotalPop,
                pm = cells$TotalPM25, prim = cells$PrimPM25, so4 = cells$pSO4, no3 = cells$pNO3, nh4 = cells$pNH4, soa = cells$SOA)
chk <- merge(P, C[, .(cell, pop, pm)], by = "cell")[, .(dpm = sum(pop * frac * pm) / sum(pop * frac)), by = zone_uid]
ref <- fread(outp("tab16_inmap_district.csv", "operating"))[, .(zone_uid, dpm25)]
chk <- merge(chk, ref, by = "zone_uid")
assert(nrow(chk) == N_DIST && chk[, max(abs(dpm - dpm25), na.rm = TRUE)] < 1e-6, "pieces do not reproduce tab16_inmap_district.csv")
f <- file.path(ROOT, "india-datacentre-ej", "data", "inmap_grid.rds")
saveRDS(list(cells = C, pieces = P), f, compress = "xz")
msg("wrote %s: %d cells, %d pieces (%.1f MB)", f, nrow(C), nrow(P), file.size(f) / 1e6)
