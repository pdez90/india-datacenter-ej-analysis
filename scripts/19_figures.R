# =============================================================================
# 19_figures.R  —  maps and charts of the main text and SI that no other script draws
# -----------------------------------------------------------------------------
# Rebuilt 3 Oct 2026 (the original 08, 18, 19 and 19b were lost on 29 Sep). Operating scope;
# 18 Oct 2026: figure-level titles removed (the captions carry them), US spelling, S1.2 legend on two rows.
# writes to paper/figs (dir_paperfig). Every number drawn is read from an output table.
#   (Figure 1, ms_fig1_inventory_flow.png, is drawn by 19b_figure1.py from the same tables.)
#   ms_fig2_concentration.png   Figure 2   facilities per district, proportional symbols, six largest labelled
#   ms_geocode_provenance.png   Fig S1.1   coordinate provenance (geocode_class) with jitter
#   ms_types.png                Fig S1.2   top-12 hosting districts by operator type: counts and allocated MW
#   ms_env_profile.png          Fig S2.1   all-India standardized differences, environmental variables
#   ms_footprint.png            Fig S4.1   footprint under the three anchors: water (left), CO2 (right)
#   ms_group_exposure.png       Fig S6.1   group-weighted exposure relative to the average Indian
#   ms_map_pm25_clean.png       Fig S6.2   district PM2.5 (LGHAP if present, else ACAG) with hosting districts
#   ms_map_water_clean.png      Fig S7.2.1 district baseline water stress with hosting districts
#   ms_grid_shadow_clean.png    Fig S8.1   district coal capacity with hosting districts
#   ms_policy_map.png           Fig S9.1   state policy year, facilities (fixed-seed jitter)
# =============================================================================
source(file.path(Sys.getenv("DCEJ_ROOT", unset = (function(){a<-commandArgs(FALSE);f<-sub("^--file=","",grep("^--file=",a,value=TRUE));d<-if(length(f)) dirname(normalizePath(f[1])) else getwd();while(!file.exists(file.path(d,"scripts","00_config.R"))&&d!=dirname(d)) d<-dirname(d);d})()), "scripts", "00_config.R"))
suppressPackageStartupMessages({ library(ggplot2); library(grid) })
assert(DC_SCOPE == "operating", "19 draws the operating-scope figures")
set.seed(20261003)

TYPE_COL <- c(hyperscale = "#b2182b", colocation = "#2166ac", telecom = "#66a61e", enterprise = "#e6ab02", government = "#6a3d9a")
thm <- theme_minimal(base_size = 12) + theme(panel.grid.minor = element_blank(), plot.title = element_text(face = "bold"), plot.title.position = "plot")
save <- function(p, f, w, h) { ggsave(file.path(dir_paperfig, f), p, width = w, height = h, dpi = 400, bg = "white"); msg("wrote %s", f) }

dist <- read_districts()
fr   <- fread(path_frame)
inv  <- fread(out("dc_current_inventory.csv"))[is_operating == TRUE | is_operating == 1]
fx   <- fread(out("tab15_facilities.csv"))
md   <- merge(dist[, "zone_uid"], fr, by = "zone_uid")
host_pts <- suppressWarnings(st_point_on_surface(md[md$hosting == 1, ]))
fac_sf <- st_transform(st_as_sf(inv, coords = c("lon", "lat"), crs = 4326), st_crs(dist))
base_map <- function() ggplot() + coord_sf(datum = NA) + theme_void(base_size = 12) +
  theme(plot.title = element_text(face = "bold"), legend.position = c(0.86, 0.28))

# ---- Figure 2: concentration ---------------------------------------------------------------------
cnt <- inv[, .N, by = .(zone_uid, dist_name)]
cp <- merge(host_pts[, c("zone_uid", "geometry")], cnt, by = "zone_uid")
top6 <- cnt[order(-N)][1:6]
lab <- cp[cp$zone_uid %in% top6$zone_uid, ]; lab$label <- sprintf("%s (%d)", lab$dist_name, lab$N)
# labels to the right of the points, spread vertically so that neighbouring metros do not overlap
xy <- st_coordinates(lab); o <- order(xy[, 2]); ly <- xy[o, 2]
for (i in seq_along(ly)[-1]) ly[i] <- max(ly[i], ly[i - 1] + 9e4)
lab_df <- data.frame(x = xy[o, 1], y = xy[o, 2], lx = xy[o, 1] + 3e5, ly = ly, label = lab$label[o])
p2 <- base_map() + geom_sf(data = dist, fill = "grey96", colour = "grey85", linewidth = 0.08) +
  geom_sf(data = cp, aes(size = N), shape = 21, fill = "#2166ac", colour = "white", alpha = 0.85) +
  geom_segment(data = lab_df, aes(x = x, y = y, xend = lx, yend = ly), colour = "grey45", linewidth = 0.3) +
  geom_text(data = lab_df, aes(x = lx, y = ly, label = label), size = 3.3, hjust = -0.05, fontface = "bold", colour = "grey15") +
  scale_size_area(max_size = 14, breaks = c(1, 5, 15, 30), name = "Operating\nfacilities")   # 246 facilities in 61 of 641 districts (caption)
save(p2, "ms_fig2_concentration.png", 8.5, 9.2)

# ---- Figure 1: inventory flow (v2) is drawn by scripts/19b_figure1.py (matplotlib; counts read from the same tables) ----

# ---- Figure S1.1: coordinate provenance -------------------------------------------------------------
gp <- copy(inv); gp[, cls := fcase(geocode_class == "dcm_city", "City centroid (directory)", geocode_class == "census_city", "City centroid (census / added records)",
                                   geocode_class == "building", "Site (building)", geocode_class == "campus", "Site (campus / IT park)",
                                   geocode_class == "locality", "Locality", geocode_class == "city", "City only",
                                   default = "Named locality")]
gsf <- st_transform(st_as_sf(gp, coords = c("lon", "lat"), crs = 4326), st_crs(dist))
gsf <- st_jitter(gsf, amount = 9000)
pS11 <- base_map() + geom_sf(data = dist, fill = "grey97", colour = "grey88", linewidth = 0.06) +
  geom_sf(data = gsf, aes(colour = cls), size = 1.4, alpha = 0.8) +
  scale_colour_manual(values = c("Site (building)" = "#2166ac", "Site (campus / IT park)" = "#4393c3", "Locality" = "#d6604d", "City only" = "#4daf4a",
                                 "City centroid (directory)" = "#d6604d", "City centroid (census / added records)" = "#2166ac", "Named locality" = "#4daf4a"), name = NULL) +
  labs(title = NULL)   # caption: 246 operating facilities on 220 distinct points, jittered ~9 km
save(pS11, "ms_geocode_provenance.png", 8.5, 9)
fwrite(gp[, .(facilities = .N, distinct_points = uniqueN(paste(lon, lat))), by = cls], out("tab19_geocode_provenance.csv"))

# ---- Figure S1.2: types in the top-12 districts ------------------------------------------------------------
top12z <- cnt[order(-N)][1:12]                       # select by zone_uid: district names repeat across states (e.g. Raigarh)
ty <- fx[status == "operating" & zone_uid %in% top12z$zone_uid, .(facilities = .N, mw = sum(mw_it)), by = .(zone_uid, dc_type)]
ty <- merge(ty, top12z[, .(zone_uid, dist_name)], by = "zone_uid")
ty[, dist_name := factor(dist_name, levels = rev(top12z$dist_name))]; ty[, dc_type := factor(dc_type, levels = names(TYPE_COL))]
pa <- ggplot(ty, aes(facilities, dist_name, fill = dc_type)) + geom_col() + scale_fill_manual(values = TYPE_COL, name = NULL, drop = FALSE) +
  labs(title = "a  Facilities", x = NULL, y = NULL) + thm + theme(legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))
pb <- ggplot(ty, aes(mw, dist_name, fill = dc_type)) + geom_col() + scale_fill_manual(values = TYPE_COL, guide = "none") +
  labs(title = "b  Allocated IT capacity (MW, central)", x = NULL, y = NULL) + thm + theme(axis.text.y = element_blank())
png(file.path(dir_paperfig, "ms_types.png"), width = 4400, height = 2200, res = 400)
grid.newpage(); pushViewport(viewport(layout = grid.layout(1, 2, widths = c(0.56, 0.44))))
print(pa, vp = viewport(layout.pos.col = 1)); print(pb, vp = viewport(layout.pos.col = 2)); invisible(dev.off())
fwrite(ty, out("tab19_types_top12.csv"))

# ---- Figure S2.1: environmental profile ---------------------------------------------------------------------
t1 <- fread(out("tab1_dc_vs_nodc_profile.csv"))
env <- t1[grepl("PM2.5|NO2|ozone|HCHO|temperature|Hot days", variable)]
env[, variable := factor(variable, levels = env[order(d_all_india), variable])]
pS21 <- ggplot(env, aes(d_all_india, variable, fill = d_all_india > 0)) + geom_col(width = 0.65) + geom_vline(xintercept = 0, colour = "grey30") +
  geom_text(aes(label = sprintf("%+.2f", d_all_india), hjust = ifelse(d_all_india > 0, -0.15, 1.15)), size = 3.4) +
  scale_fill_manual(values = c(`TRUE` = "#b2182b", `FALSE` = "#2166ac"), guide = "none") +
  scale_x_continuous(expand = expansion(mult = 0.25)) +
  labs(x = "Standardized difference d, hosting minus non-hosting districts (positive = higher in hosting districts)", y = NULL) + thm
save(pS21, "ms_env_profile.png", 8, 4.6)

# ---- Figure S4.1: footprint under the three anchors -------------------------------------------------------------
nat <- fread(out("tab15_national.csv"))[, scenario := factor(scenario, levels = c("low", "central", "high"))]
wl <- melt(nat[, .(scenario, `Scope 1 (on-site cooling)` = scope1_gl, `Scope 2 (electricity)` = scope2_gl)], id.vars = "scenario")
pw <- ggplot(wl, aes(scenario, value, fill = variable)) + geom_col() +
  geom_point(data = nat, aes(scenario, scope1_gl + scope2_gl_nohydro), inherit.aes = FALSE, shape = 4, size = 3, stroke = 1.2) +
  scale_fill_manual(values = c("#9ecae1", "#2171b5"), name = NULL) + labs(title = "Water (GL/yr)", x = "Capacity anchor scenario", y = NULL,
  caption = "x = total with hydropower evaporation set to zero") + thm + theme(legend.position = "bottom")
pc <- ggplot(nat, aes(scenario, co2_mt)) + geom_col(fill = "grey45") +
  geom_text(aes(label = sprintf("%.0f deaths/yr", coal_fleet_deaths)), vjust = -0.4, size = 3.3) +
  labs(title = "CO2 (Mt/yr, average grid factor)", x = "Capacity anchor scenario", y = NULL, caption = "labels: proportional coal-fleet mortality") + thm
png(file.path(dir_paperfig, "ms_footprint.png"), width = 4400, height = 2000, res = 400)
grid.newpage(); pushViewport(viewport(layout = grid.layout(1, 2)))
print(pw, vp = viewport(layout.pos.col = 1)); print(pc, vp = viewport(layout.pos.col = 2)); invisible(dev.off())

# ---- Figure S6.1: group-weighted exposure ----------------------------------------------------------------------
ge <- melt(fread(out("tab20a_group_exposure_S61.csv"))[, !"population_million"], id.vars = "group")
ge[, group := factor(group, levels = rev(unique(group)))]
pS61 <- ggplot(ge, aes(value, group, fill = value >= 1)) + geom_col(width = 0.65) + geom_vline(xintercept = 1, linetype = "dashed") +
  facet_wrap(~variable, nrow = 1) + scale_fill_manual(values = c(`TRUE` = "#b2182b", `FALSE` = "#2166ac"), guide = "none") +
  labs(x = "Group-weighted exposure relative to the average Indian (= 1)", y = NULL) + thm
save(pS61, "ms_group_exposure.png", 11, 4.8)

# ---- district maps with hosting districts -----------------------------------------------------------------------
pmv <- if ("pm25_lghap" %in% names(md) && any(is.finite(md$pm25_lghap))) "pm25_lghap" else "pm25_acag_local"
pS62 <- base_map() + geom_sf(data = md, aes(fill = .data[[pmv]]), colour = NA) +
  geom_sf(data = host_pts, shape = 21, fill = "white", colour = "black", size = 1.8) +
  scale_fill_viridis_c(option = "inferno", direction = -1, name = "PM2.5\n(\u00b5g/m\u00b3)", na.value = "grey90") +
  labs(title = NULL)   # caption names the product (LGHAP v2, 2017-2021) and the 61 hosting districts
save(pS62, "ms_map_pm25_clean.png", 8.5, 9)

md$bws_class <- cut(md$bws_raw, c(-Inf, 0.1, 0.2, 0.4, 0.8, Inf), labels = c("Low", "Low-medium", "Medium-high", "High", "Extremely high"))
pS721 <- base_map() + geom_sf(data = md, aes(fill = bws_class), colour = NA) +
  geom_sf(data = host_pts, shape = 21, fill = "#2166ac", colour = "white", size = 2) +
  scale_fill_manual(values = c("#ffffcc", "#ffeda0", "#feb24c", "#f03b20", "#bd0026"), na.value = "grey90", name = "Baseline water\nstress (Aqueduct 4.0)") +
  labs(title = NULL)
save(pS721, "ms_map_water_clean.png", 8.5, 9)

pS81 <- base_map() + geom_sf(data = md, aes(fill = ifelse(coal_mw > 0, coal_mw, NA)), colour = NA) +
  geom_sf(data = host_pts, shape = 21, fill = "#2166ac", colour = "white", size = 2) +
  scale_fill_distiller(palette = "Greys", direction = 1, trans = "sqrt", na.value = "grey97", name = "Coal capacity\n(MW)") +
  labs(title = NULL)
save(pS81, "ms_grid_shadow_clean.png", 8.5, 9)

# ---- Figure S9.1: policy map -------------------------------------------------------------------------------------
pol <- fread(out("tab14_policy_burden.csv"))
stn <- function(s) { s <- gsub("Delhi \\(NCT\\)", "Nct Of Delhi", s); gsub("^Chhattisgarh$", "Chattisgarh", s) }
st_shp <- md; st_shp$state_cur <- st_shp$state_name   # frame state_name already carries Telangana
st_shp <- aggregate(st_shp[, "geometry"], by = list(state = st_shp$state_cur), FUN = function(x) x[1])
st_shp <- merge(st_shp, pol[has_policy == TRUE, .(state = stn(state), policy_year)], by = "state", all.x = TRUE)
fj <- st_jitter(fac_sf, amount = 0.08 * 111000)
pS91 <- base_map() + geom_sf(data = st_shp, aes(fill = policy_year), colour = "white", linewidth = 0.2) +
  geom_sf(data = fj, size = 1, colour = "#08306b", alpha = 0.8) +
  scale_fill_distiller(palette = "YlOrBr", direction = 1, na.value = "grey88", name = "Policy year\n(grey = none)",
                       breaks = c(2016, 2019, 2022, 2025), labels = function(x) sprintf("%d", as.integer(x))) +
  labs(title = NULL)
msg("S9.1: %d of %d operating facilities sit in states with a dedicated policy", pol[has_policy == TRUE, sum(dc_count)], nrow(inv))
save(pS91, "ms_policy_map.png", 8.5, 9)
msg("19 done")
