# =============================================================================
# 16f_inmap_figures.R  —  figures of the InMAP receiving field (main Figure 3, SI Figures S4.1.1, S8.2)
# -----------------------------------------------------------------------------
# Rebuilt 1 Oct 2026 from the published figures (the original script was lost with the pipeline).
#   ms_inmap_key.png          main Figure 3: (a) district PM2.5 increment (log10, magma, white points =
#                             facilities of this scope); (b) group ratios to the national pw mean
#                             (Urban, SC/ST, BPL, Rural); (c) district wealth quintiles Q1-Q5.
#                             Bars red above 1, blue below; dashed line at 1.
#   fig16_inmap_receiving.png SI Figure S4.1.1: the map of panel (a) on its own, full page.
#   ms_plants.png             SI Figure S8.2: the coal fleet (GPPD, 253 plants), circles sized by MW.
# Inputs (dir_out of this scope): tab16_inmap_district.csv (16d), tab16e_group_exposure.csv (16e),
#   dc_current_inventory.csv (01; operating facilities only under DC_SCOPE=operating),
#   tab16b_emissions_points.csv (16b).
# Where: operating -> paper/figs (ms_*) and outputs/figures (fig16_*); all -> outputs_all/figures only,
#   so the all-scope run cannot overwrite the publication figures (REBUILD_SPEC 0.3).
# x-axis patch kept from the original: limits c(0, max(1.75, 1.15 x largest ratio)), shared by (b) and (c),
#   whose plot areas are aligned; value labels sit on a white box so the dashed line cannot cross them.
# Colour-scale range: 10^floor(log10(0.5th pct of districts)) to the district maximum; values below are squished.
# =============================================================================
source(file.path(Sys.getenv("DCEJ_ROOT", unset = (function(){a<-commandArgs(FALSE);f<-sub("^--file=","",grep("^--file=",a,value=TRUE));d<-if(length(f)) dirname(normalizePath(f[1])) else getwd();while(!file.exists(file.path(d,"scripts","00_config.R"))&&d!=dirname(d)) d<-dirname(d);d})()), "scripts", "00_config.R"))
suppressPackageStartupMessages({ library(ggplot2); library(grid) })

RED <- "#B2182B"; BLUE <- "#2166AC"
theme_map <- theme_void(base_size = 13) + theme(legend.position = c(0.86, 0.30), legend.title = element_text(size = 13),
                                                plot.title = element_text(face = "bold", size = 17))

# ---- inputs ------------------------------------------------------------------------------------
dist <- read_districts()
d16  <- fread(out("tab16_inmap_district.csv"))[, .(zone_uid, dpm25)]
assert(nrow(d16) >= 640, "tab16_inmap_district.csv has %d rows: run 16d for DC_SCOPE=%s first", nrow(d16), DC_SCOPE)
summ <- readLines(out("tab16_inmap_summary.txt"))
assert(any(grepl(sprintf("DC_SCOPE=%s, INVENTORY=%s", DC_SCOPE, INVENTORY), summ, fixed = TRUE)),
       "tab16_inmap_summary.txt is not from DC_SCOPE=%s, INVENTORY=%s", DC_SCOPE, INVENTORY)
md <- merge(dist, d16, by = "zone_uid", all.x = TRUE)
assert(sum(!is.na(md$dpm25)) >= 640, "only %d districts carry an increment", sum(!is.na(md$dpm25)))

fac <- fread(out("dc_current_inventory.csv"))
if (DC_SCOPE == "operating" && "is_operating" %in% names(fac)) fac <- fac[is_operating == TRUE | is_operating == 1]
fac <- fac[is.finite(lon) & is.finite(lat)]
fac_sf <- st_transform(st_as_sf(fac, coords = c("lon", "lat"), crs = 4326), st_crs(dist))
msg("%d facilities plotted (%s)", nrow(fac), DC_SCOPE)

ge <- fread(out("tab16e_group_exposure.csv"))

# ---- (a) map -------------------------------------------------------------------------------------
pos <- md$dpm25[is.finite(md$dpm25) & md$dpm25 > 0]
lims <- c(10^floor(log10(quantile(pos, 0.005))), max(pos))
map_a <- function(title = NULL) {
  ggplot() +
    geom_sf(data = md, aes(fill = dpm25), colour = scales::alpha("white", 0.25), linewidth = 0.05) +
    geom_sf(data = fac_sf, shape = 21, fill = "white", colour = "black", size = 1.6, stroke = 0.35) +
    scale_fill_viridis_c(option = "magma", direction = -1, trans = "log10", limits = lims, oob = scales::squish,
                         labels = function(x) format(x, scientific = TRUE), na.value = "grey85",
                         name = expression(Delta * PM[2.5] ~ (mu * g ~ m^-3))) +
    guides(fill = guide_colourbar(barheight = unit(5.5, "cm"), barwidth = unit(0.8, "cm"))) +
    coord_sf(datum = NA) + theme_map + labs(title = title)
}

# ---- (b, c) bars ---------------------------------------------------------------------------------
XMAX <- max(1.75, max(ge[group %in% c("Urban", "SC/ST", "BPL", "Rural", "Q1 (poorest)", "Q2", "Q3", "Q4", "Q5 (richest)"), ratio_to_national]) * 1.15)
bars <- function(groups, labels, title, xlab = NULL) {
  dat <- ge[match(groups, group)][, label := factor(labels, levels = rev(labels))]
  assert(!anyNA(dat$ratio_to_national), "16e lacks %s", paste(groups[is.na(dat$ratio_to_national)], collapse = ", "))
  dat[, side := fifelse(ratio_to_national >= 1, "above", "below")]
  ggplot(dat, aes(ratio_to_national, label, fill = side)) +
    geom_vline(xintercept = 1, linetype = "dashed", colour = "grey30", linewidth = 0.6) +
    geom_col(width = 0.62) +
    geom_label(aes(label = sprintf("%.2f", ratio_to_national)), hjust = -0.12, size = 4.6, fill = "white",
               label.size = 0, label.padding = unit(0.08, "lines")) +
    scale_fill_manual(values = c(above = RED, below = BLUE), guide = "none") +
    scale_x_continuous(limits = c(0, XMAX), breaks = seq(0, 3, 0.5), expand = c(0, 0)) +
    labs(title = title, x = xlab, y = NULL) +
    theme_minimal(base_size = 13) +
    theme(panel.grid = element_blank(), plot.title = element_text(face = "bold", size = 15),
          axis.text.y = element_text(colour = "grey25"), axis.text.x = element_text(colour = "grey25"))
}
pb <- bars(c("Urban", "SC/ST", "BPL", "Rural"), c("Urban", "SC/ST", "Below-poverty-line", "Rural"),
           "b  Exposure by group (ratio to national mean = 1)")
pc <- bars(c("Q1 (poorest)", "Q2", "Q3", "Q4", "Q5 (richest)"), c("Q1 (poorest)", "Q2", "Q3", "Q4", "Q5 (richest)"),
           "c  Exposure by district wealth quintile", "increment relative to national population-weighted mean")

# ---- write ---------------------------------------------------------------------------------------
f_key <- file.path(dir_paperfig, "ms_inmap_key.png")
png(f_key, width = 2046, height = 1153, res = 150)
grid.newpage(); pushViewport(viewport(layout = grid.layout(2, 2, widths = c(0.5, 0.5))))
print(map_a("a  Where the attributed PM2.5 lands"), vp = viewport(layout.pos.row = 1:2, layout.pos.col = 1))
gb <- ggplotGrob(pb); gc <- ggplotGrob(pc); w <- unit.pmax(gb$widths, gc$widths); gb$widths <- w; gc$widths <- w
pushViewport(viewport(layout.pos.row = 1, layout.pos.col = 2)); grid.draw(gb); popViewport()
pushViewport(viewport(layout.pos.row = 2, layout.pos.col = 2)); grid.draw(gc); popViewport()
invisible(dev.off())

f_rec <- file.path(dir_fig, "fig16_inmap_receiving.png")
ggsave(f_rec, map_a() + theme(legend.position = c(0.88, 0.28)), width = 9.6, height = 10.5, dpi = 200, bg = "white")

pl <- fread(out("tab16b_emissions_points.csv"))[fuel == "Coal" & is.finite(lon)]
assert(nrow(pl) == 253, "expected 253 coal plants, found %d", nrow(pl))
pl_sf <- st_transform(st_as_sf(pl, coords = c("lon", "lat"), crs = 4326), st_crs(dist))
f_pl <- file.path(dir_paperfig, "ms_plants.png")
ggsave(f_pl, ggplot() +
         geom_sf(data = dist, fill = "grey96", colour = "grey88", linewidth = 0.1) +
         geom_sf(data = pl_sf[order(-pl_sf$capacity_mw), ], aes(size = capacity_mw), colour = "grey30", alpha = 0.7) +
         scale_size_area(max_size = 16, breaks = c(500, 2000, 4000), name = "Plant\ncapacity (MW)") +
         coord_sf(datum = NA) + theme_void(base_size = 15) + theme(legend.position = c(0.86, 0.30)),
       width = 9.3, height = 10.2, dpi = 200, bg = "white")

msg("colour scale %g-%g ug/m3; national pw increment %.4f; %d coal plants (%.0f GW)",
    lims[1], lims[2], ge[group == "Urban", national_mean_ugm3], nrow(pl), sum(pl$capacity_mw) / 1e3)
msg("wrote %s, %s, %s", f_key, f_rec, f_pl)
msg("16f done")
