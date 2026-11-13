# =============================================================================
# 29_backup_diesel.R  —  backup-diesel runtime and emissions (SI S5, Table S5.1, Figures S5.1-S5.4)
# -----------------------------------------------------------------------------
# Rebuilt 3 Oct 2026 (the original DCGRD module, dcgrd_00-08, was lost on 29 Sep).
# For every facility of this scope (tab15_facilities.csv, central scenario):
#   hours in which backup operation may be required   H_d = f x O_d + T
#     O_d  measured supply-interruption hours of the facility's district (28_esmi_outages.py:
#          minutes below 80 V, Prayas ESMI 2014-2019), taken by tier (decided 3 Oct 2026):
#            1 the district's urban-monitor mean            (district, urban)
#            2 the district's all-monitor mean, if it has only rural monitors
#            3 the median of the urban monitors in the facility's state, if the district has none
#            4 the national median of urban monitors, if the state has none
#          The submitted paper instead set O_d = 0 for unmonitored districts (testing hours only);
#          that rule is reported as a sensitivity ("zero-fill").
#     f    feeder-quality fraction 0.2 / 0.4 (central) / 1.0: data centres take supply on dedicated
#          high-tension feeders, better than the urban-domestic connections ESMI samples
#     T    scheduled testing 50 / 100 (central) / 150 h/yr (U.S. EPA 2013 100-h non-emergency limit)
#   diesel energy [kWh] = facility electricity [kWh/yr] / 8760 x H_d
#   emissions = energy x emission factor: NOx 8 [4-14] g/kWh (CPCB 2014 genset norms -> AP-42
#     uncontrolled), PM2.5 0.20 [0.05-0.5] g/kWh, SO2 0.005 g/kWh (BS-VI 10 ppm S), CO2 0.75 kg/kWh.
# Compared with the scope's grid-attributable marginal emissions (15b, tab15b_marginal_totals.csv).
# Outputs (dir_out): tab29_diesel_scenarios.csv (Table S5.1: f x T), tab29_diesel_district.csv,
#   tab29_diesel_summary.txt; figures (operating scope -> paper/figs): fig_dcgrd_hourofday.png (S5.1),
#   fig_dcgrd_dcdistricts.png (S5.2), fig_dcgrd_dieselvsgrid.png (S5.3), fig_dcgrd_outagemap.png (S5.4).
# Submitted paper (v1, old ESMI join, zero-fill): central 2,918 t NOx = 4.97% of grid NOx; Table S5.1
#   2.5-9.4%; EF span 2.5-8.7%; PM2.5 0.5-1.7%. Not asserted: the ESMI layer and the inventory changed.
# =============================================================================
source(file.path(Sys.getenv("DCEJ_ROOT", unset = (function(){a<-commandArgs(FALSE);f<-sub("^--file=","",grep("^--file=",a,value=TRUE));d<-if(length(f)) dirname(normalizePath(f[1])) else getwd();while(!file.exists(file.path(d,"scripts","00_config.R"))&&d!=dirname(d)) d<-dirname(d);d})()), "scripts", "00_config.R"))
suppressPackageStartupMessages(library(ggplot2))

F_SET <- c(0.2, 0.4, 1.0); T_SET <- c(50, 100, 150); F0 <- 0.4; T0 <- 100
EF <- data.table(pollutant = c("nox", "pm25", "so2", "co2"), low = c(4, 0.05, 0.005, 0.75), central = c(8, 0.20, 0.005, 0.75),
                 high = c(14, 0.5, 0.005, 0.75), unit = c("g/kWh", "g/kWh", "g/kWh", "kg/kWh"))
URBAN_MIN_DAYS <- 30

# ---- inputs ---------------------------------------------------------------------------------
dist_f <- file.path(ROOT, "outputs", "tab28_esmi_district_outages.csv")     # ESMI is scope-independent; 28 writes to outputs/
mon_f  <- file.path(dir_derived, "esmi_monitor_outages.csv")
assert(file.exists(dist_f) && file.exists(mon_f), "run scripts/28_esmi_outages.py first")
ed  <- fread(dist_f)
mon <- fread(mon_f)[used == TRUE]
mon[, state := state_fix(state_2015, district_2015)]
fx  <- fread(out("tab15_facilities.csv"))[, .(dc_id, name, zone_uid, dist_name, state_name, e_fac_gwh)]
tot <- fread(out("tab15b_marginal_totals.csv"))

nat_urban <- median(mon[urban == TRUE, interruption_h_per_yr])
st_urban  <- mon[urban == TRUE, .(state_urban_median = median(interruption_h_per_yr), state_urban_monitors = .N), by = state]

dd <- fx[, .(facilities = .N, e_gwh = sum(e_fac_gwh)), by = .(zone_uid, dist_name, state_name)]
dd <- merge(dd, ed[, .(zone_uid, n_monitors, n_urban_monitors, h_all = interruption_h_per_yr, h_urban = interruption_h_per_yr_urban)],
            by = "zone_uid", all.x = TRUE)
dd <- merge(dd, st_urban, by.x = "state_name", by.y = "state", all.x = TRUE)
dd[, `:=`(
  tier = fcase(!is.na(h_urban), 1L, !is.na(h_all), 2L, !is.na(state_urban_median), 3L, default = 4L),
  O_d  = fcase(!is.na(h_urban), h_urban, !is.na(h_all), h_all, !is.na(state_urban_median), state_urban_median, default = nat_urban))]
dd[, tier_label := c("district urban monitors", "district all monitors", "state urban median", "national urban median")[tier]]
dd[, O_zero := fifelse(tier <= 2L, O_d, 0)]                       # the submitted paper's rule (sensitivity)
assert(nrow(dd) == uniqueN(fx$zone_uid) && !anyNA(dd$O_d), "every hosting district needs an outage value")

# ---- scenarios -----------------------------------------------------------------------------------
kwh_per_h <- function(e_gwh) e_gwh * 1e6 / 8760                    # facility load, kWh per hour
diesel <- function(O, f, T, ef_nox = 8, ef_pm = 0.20) {
  kwh <- dd[, sum(kwh_per_h(e_gwh) * (f * get(O) + T))]
  c(kwh = kwh, nox_t = kwh * ef_nox / 1e6, pm25_t = kwh * ef_pm / 1e6, so2_t = kwh * 0.005 / 1e6, co2_t = kwh * 0.75 / 1e3)
}
sc <- CJ(f = F_SET, testing_h = T_SET)
for (O in c("O_d", "O_zero")) {
  r <- t(mapply(function(f, T) diesel(O, f, T), sc$f, sc$testing_h))
  tag <- if (O == "O_d") "" else "_zerofill"
  sc[, (paste0("diesel_mwh", tag)) := r[, "kwh"] / 1e3]
  sc[, (paste0("diesel_nox_t", tag)) := r[, "nox_t"]]
  sc[, (paste0("diesel_pm25_t", tag)) := r[, "pm25_t"]]
  sc[, (paste0("pct_grid_nox", tag)) := 100 * r[, "nox_t"] / (tot$nox_kt * 1e3)]
  sc[, (paste0("pct_grid_pm25", tag)) := 100 * r[, "pm25_t"] / (tot$pm25_kt * 1e3)]
}
sc[, `:=`(scope = DC_SCOPE, inventory = INVENTORY, grid_nox_t = tot$nox_kt * 1e3, grid_pm25_t = tot$pm25_kt * 1e3)]
fwrite(sc, out("tab29_diesel_scenarios.csv"))

cen <- sc[f == F0 & testing_h == T0]
ef_lo <- diesel("O_d", F0, T0, ef_nox = EF[pollutant == "nox", low]); ef_hi <- diesel("O_d", F0, T0, ef_nox = EF[pollutant == "nox", high])
pct <- function(x) 100 * x / (tot$nox_kt * 1e3)

# district table (central scenario)
dd[, `:=`(H_central = F0 * O_d + T0, diesel_mwh_central = kwh_per_h(e_gwh) * (F0 * O_d + T0) / 1e3)]
dd[, diesel_nox_t_central := diesel_mwh_central * 1e3 * 8 / 1e6]
dd[, diesel_pm25_t_central := diesel_mwh_central * 1e3 * 0.20 / 1e6]
setorder(dd, -diesel_nox_t_central)
fwrite(cbind(scope = DC_SCOPE, inventory = INVENTORY, dd), out("tab29_diesel_district.csv"))

# ---- summary ---------------------------------------------------------------------------------------
tier_share <- dd[, .(districts = .N, share_electricity = sum(e_gwh) / dd[, sum(e_gwh)]), by = tier_label][order(-share_electricity)]
lines <- c(
  sprintf("Backup diesel (29), DC_SCOPE=%s, INVENTORY=%s, %s", DC_SCOPE, INVENTORY, format(Sys.time(), "%Y-%m-%d %H:%M")),
  sprintf("Hosting districts: %d; outage hours by tier: %s", nrow(dd),
          paste(sprintf("%s %d (%.0f%% of electricity)", tier_share$tier_label, tier_share$districts, 100 * tier_share$share_electricity), collapse = "; ")),
  sprintf("National urban-monitor median %.0f h/yr (fallback tier 4)", nat_urban),
  sprintf("Central (f %.1f, testing %d h): diesel %.0f MWh, NOx %.0f t = %.2f%% of grid-attributable marginal NOx (%.1f kt); PM2.5 %.1f t = %.2f%% of grid PM2.5",
          F0, T0, cen$diesel_mwh, cen$diesel_nox_t, cen$pct_grid_nox, tot$nox_kt, cen$diesel_pm25_t, cen$pct_grid_pm25),
  sprintf("Across f x testing at central EFs: NOx %.1f-%.1f%% of grid NOx; PM2.5 %.2f-%.2f%%",
          min(sc$pct_grid_nox), max(sc$pct_grid_nox), min(sc$pct_grid_pm25), max(sc$pct_grid_pm25)),
  sprintf("NOx emission-factor range (4-14 g/kWh) at the central point: %.1f-%.1f%%", pct(ef_lo[["nox_t"]]), pct(ef_hi[["nox_t"]])),
  sprintf("Sensitivity, zero-fill rule of inventory v1 (unmonitored districts counted at zero measured hours, testing only): central %.2f%%, range %.1f-%.1f%%",
          cen$pct_grid_nox_zerofill, min(sc$pct_grid_nox_zerofill), max(sc$pct_grid_nox_zerofill)),
  sprintf("Central diesel CO2 %.3f Mt; SO2 %.2f t", cen$diesel_mwh * 0.75 / 1e6, cen$diesel_mwh * 1e3 * 0.005 / 1e6))
writeLines(lines, out("tab29_diesel_summary.txt")); cat(lines, sep = "\n")
assert(cen$pct_grid_nox > 0 && cen$pct_grid_nox < 20, "central diesel share %.2f%% outside 0-20%%: check units", cen$pct_grid_nox)
assert(abs(sum(dd$e_gwh) - sum(fx$e_fac_gwh)) < 1e-6, "district spine does not carry all facility electricity")

# ---- figures (S5.1-S5.4) ------------------------------------------------------------------------------
th <- theme_minimal(base_size = 12) + theme(panel.grid.minor = element_blank(), plot.title = element_text(face = "bold"))
hod <- melt(fread(file.path(ROOT, "outputs", "tab28_esmi_hourofday.csv")), id.vars = "hour", variable.name = "monitors", value.name = "share")
p1 <- ggplot(hod, aes(hour, 100 * share, colour = monitors)) + geom_line(linewidth = 1) + geom_point(size = 1.4) +
  scale_colour_manual(values = c(rural = "#B2182B", urban = "#2166AC"), labels = c(rural = "Rural (gram panchayat)", urban = "Urban")) +
  scale_x_continuous(breaks = seq(0, 23, 3)) +
  labs(title = "Supply interruptions by hour of day", x = "Hour of day (local)", y = "Monitored minutes below 80 V (%)", colour = NULL,
       caption = "Prayas ESMI minute-wise voltage, 2014-2019") + th + theme(legend.position = "top") + labs(title = NULL)
ggsave(file.path(dir_paperfig, "fig_dcgrd_hourofday.png"), p1, width = 7.5, height = 4.2, dpi = 400, bg = "white")

hd <- dd[tier <= 2][order(O_d)][, lab := factor(paste0(dist_name, ", ", state_name), levels = paste0(dist_name, ", ", state_name))]
rural_mean <- ed[!is.na(interruption_h_per_yr_rural), mean(interruption_h_per_yr_rural)]   # district means, as in the S5.2 caption (~2,000 h/yr)
p2 <- ggplot(hd, aes(O_d, lab)) + geom_col(fill = "#2166AC", width = 0.7) +
  geom_vline(xintercept = nat_urban, linetype = "dashed", colour = "grey30") +
  annotate("text", x = nat_urban, y = 0.6, label = sprintf(" national urban median %.0f h", nat_urban), hjust = 0, vjust = 0, size = 3.2) +
  labs(title = NULL,   # caption: hosting districts with an urban ESMI monitor; rural monitors average ~2,000 h/yr (district means)
       x = "Supply-interruption hours per year\n(urban-monitor mean; minutes below 80 V)", y = NULL) + th +
  theme(plot.title.position = "plot")
ggsave(file.path(dir_paperfig, "fig_dcgrd_dcdistricts.png"), p2, width = 8.5, height = 1.6 + 0.22 * nrow(hd), dpi = 400, bg = "white")

p3 <- ggplot(sc, aes(factor(testing_h), pct_grid_nox, fill = factor(f))) + geom_col(position = position_dodge(0.75), width = 0.7) +
  geom_text(aes(label = sprintf("%.1f", pct_grid_nox)), position = position_dodge(0.75), vjust = -0.3, size = 3.4) +
  scale_fill_manual(values = c("0.2" = "#92C5DE", "0.4" = "#4393C3", "1" = "#2166AC"), name = "Feeder-quality\nfraction f") +
  labs(title = NULL,   # caption: 4-14 g/kWh moves the central point over 2.8-9.9%
       x = "Scheduled testing (h/yr)", y = "Diesel NOx (% of grid-attributable marginal NOx)") + th
ggsave(file.path(dir_paperfig, "fig_dcgrd_dieselvsgrid.png"), p3, width = 7.5, height = 4.6, dpi = 400, bg = "white")

dshp <- read_districts()
md <- merge(dshp, ed[, .(zone_uid, h = fifelse(is.na(interruption_h_per_yr_urban), interruption_h_per_yr, interruption_h_per_yr_urban),
                         hosting = hosting_operating)], by = "zone_uid", all.x = TRUE)
host_pts <- suppressWarnings(st_point_on_surface(md[!is.na(md$hosting) & md$hosting == TRUE, ]))
p4 <- ggplot() + geom_sf(data = md, aes(fill = h), colour = "grey80", linewidth = 0.05) +
  geom_sf(data = host_pts, shape = 21, fill = "#D7301F", colour = "black", size = 2.2) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt", na.value = "grey92", name = "Interruption\nhours / yr") +
  labs(title = "a") + coord_sf(datum = NA) + theme_void(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 15), legend.position = c(0.88, 0.30))
# (b) urban-monitor interruption hours, hosting vs non-hosting districts (the S5.4 caption's medians and IQRs)
ub <- ed[!is.na(interruption_h_per_yr_urban), .(h = interruption_h_per_yr_urban,
          grp = factor(ifelse(hosting_operating == TRUE, "Hosting", "Not hosting"), levels = c("Hosting", "Not hosting")))]
nlab <- ub[, .N, by = grp][order(grp), setNames(sprintf("%s\n(%d districts)", grp, N), as.character(grp))]
ub[, grp_lab := factor(nlab[as.character(grp)], levels = unname(nlab))]
set.seed(20261018)
p4b <- ggplot(ub, aes(grp_lab, h)) +
  geom_boxplot(width = 0.5, outlier.shape = NA, fill = "grey95", colour = "grey30") +
  geom_jitter(aes(fill = grp), width = 0.12, height = 0, shape = 21, colour = "black", size = 2, stroke = 0.3) +
  scale_fill_manual(values = c(Hosting = "#D7301F", `Not hosting` = "grey70"), guide = "none") +
  scale_y_log10(breaks = c(10, 30, 100, 300, 1000, 3000), labels = scales::comma) +
  labs(title = "b", x = NULL, y = "Supply-interruption hours per year\n(urban-monitor mean, log scale)") + th +
  theme(plot.title = element_text(face = "bold", size = 15), panel.grid.major.x = element_blank())
msg("S5.4b: hosting median %.0f (IQR %.0f-%.0f, max %.0f), n %d; not hosting median %.0f (IQR %.0f-%.0f, max %.0f), n %d",
    ub[grp == "Hosting", median(h)], ub[grp == "Hosting", quantile(h, 0.25)], ub[grp == "Hosting", quantile(h, 0.75)], ub[grp == "Hosting", max(h)], ub[grp == "Hosting", .N],
    ub[grp != "Hosting", median(h)], ub[grp != "Hosting", quantile(h, 0.25)], ub[grp != "Hosting", quantile(h, 0.75)], ub[grp != "Hosting", max(h)], ub[grp != "Hosting", .N])
png(file.path(dir_paperfig, "fig_dcgrd_outagemap.png"), width = 5200, height = 3000, res = 400)
grid::grid.newpage(); grid::pushViewport(grid::viewport(layout = grid::grid.layout(1, 2, widths = c(0.66, 0.34))))
print(p4, vp = grid::viewport(layout.pos.col = 1)); print(p4b, vp = grid::viewport(layout.pos.col = 2)); invisible(dev.off())
msg("29 done")
