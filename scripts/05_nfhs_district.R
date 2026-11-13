# =============================================================================
# 05_nfhs_district.R  —  NFHS-5 (2019-21) district social composition
# -----------------------------------------------------------------------------
# Person-recode file (IAPR7DFL, 2.84 M household members), design-weighted by
# hv005. Clusters are placed on the 2015 NFHS district geography with the
# NFHS-5 GPS file (IAGE7AFL, displaced cluster centroids): each cluster goes to
# the district polygon it falls in; a cluster that falls in no polygon (coastal
# or border displacement, offset city polygons) goes to the nearest polygon if
# that is within 10 km, and is dropped otherwise. Clusters with missing
# coordinates (0,0) are dropped. All shares are household-member weighted,
# i.e. population shares.
#
# Validated 30 Sep 2026 against the surviving app layer: this rule reproduces
# the published district values exactly (to 1e-6) for 634 of 635 districts on
# wealth, urban share, SC/ST, Muslim, no-electricity and BPL (without the 10 km
# cap: 603). The one residual is Rangareddy (DIST_0537): 968,409 here vs
# 935,886 published; the original placement of a few Hyderabad-area clusters
# could not be recovered. Chennai (offshore polygon) gets no clusters, as before.
#
# Coding (NFHS-5 PR recode):
#   hv025 1 urban | hv206 0 no electricity | sh47 1 Hindu, 2 Muslim
#   sh49 1 SC, 2 ST, 3 OBC, 4 none ("General"); denominators include missing and
#        don't-know for the combined share_scst_2019 (reproduces the app layer
#        exactly), but the single categories SC, ST, OBC and General use valid
#        responses (1-4) as denominator: this reproduces Table 1's ST shares
#        (0.046 / 0.148 / 0.080) and the SC/OBC subgroup burdens of 13_burden.
#        The inconsistency is inherited from the original and noted in README.
#   sh75 BPL card 1 yes, 0 no, 8 don't know -> share among 0/1 only
#   hv271 wealth-index factor score (5 implied decimals; kept in raw units)
#   hv270 wealth quintile 1 poorest .. 5 richest
# =============================================================================
source(file.path(Sys.getenv("DCEJ_ROOT", unset = (function(){a<-commandArgs(FALSE);f<-sub("^--file=","",grep("^--file=",a,value=TRUE));d<-if(length(f)) dirname(normalizePath(f[1])) else getwd();while(!file.exists(file.path(d,"scripts","00_config.R"))&&d!=dirname(d)) d<-dirname(d);d})()), "scripts", "00_config.R"))

cols <- c("hv001","hv005","hv024","hv025","hv206","hv270","hv271","sh47","sh49","sh75")
csv_override <- Sys.getenv("DCEJ_NFHS5_PR_CSV", unset = "")   # dev/testing: a column subset as CSV
if (csv_override != "") {
  pr <- fread(csv_override, select = cols)
} else {
  assert(file.exists(path_nfhs5_pr), "NFHS-5 PR file not found: %s", path_nfhs5_pr)
  msg("reading %s (this takes a few minutes)", basename(path_nfhs5_pr))
  pr <- as.data.table(haven::zap_labels(haven::read_dta(path_nfhs5_pr, col_select = dplyr::all_of(cols))))
}
num <- c("hv005","hv271"); pr[, (num) := lapply(.SD, as.numeric), .SDcols = num]   # avoid integer overflow
msg("NFHS-5 persons: %s", format(nrow(pr), big.mark = ","))
assert(nrow(pr) > 2.8e6, "expected ~2.84 M person records")

# ---- clusters -> 2015 districts ----------------------------------------------------
dist <- read_districts()
gps  <- st_read(path_nfhs5_gps, quiet = TRUE)
gps  <- gps[gps$LATNUM != 0 | gps$LONGNUM != 0, "DHSCLUST"]
gps  <- st_transform(gps, st_crs(dist))
NEAREST_CAP_M <- 10000
inside <- vapply(st_within(gps, dist), function(i) if (length(i)) i[1] else NA_integer_, 1L)
near   <- st_nearest_feature(gps, dist)
near_m <- as.numeric(st_distance(gps, dist[near, ], by_element = TRUE))
pick   <- ifelse(!is.na(inside), inside, ifelse(near_m <= NEAREST_CAP_M, near, NA_integer_))
cl   <- data.table(hv001 = gps$DHSCLUST, zone_uid = dist$zone_uid[pick], placement = fifelse(!is.na(inside), "within", "nearest<=10km"))
cl   <- cl[!is.na(zone_uid) & !duplicated(hv001)]
msg("clusters placed: %d (%d by nearest <= 10 km; %d dropped)", nrow(cl), cl[placement != "within", .N], sum(is.na(pick)))
fwrite(cl, file.path(dir_derived, "nfhs5_cluster_district.csv"))

pr <- merge(pr, cl, by = "hv001")
wm <- function(x, w) sum(x * w) / sum(w)

ses <- pr[, .(
  n_persons            = .N,
  n_clusters           = uniqueN(hv001),
  wealth_mean_2019     = wm(hv271, hv005),
  urban_share_2019     = wm(hv025 == 1, hv005),
  share_sc_2019        = { k <- sh49 %in% 1:4; wm(sh49[k] == 1, hv005[k]) },   # valid-response denominator
  share_st_2019        = { k <- sh49 %in% 1:4; wm(sh49[k] == 2, hv005[k]) },   # valid-response denominator (Table 1)
  share_scst_2019      = wm(sh49 %in% 1:2, hv005),
  share_obc_2019       = { k <- sh49 %in% 1:4; wm(sh49[k] == 3, hv005[k]) },   # valid-response denominator
  share_general_2019   = { k <- sh49 %in% 1:4; wm(sh49[k] == 4, hv005[k]) },   # valid-response denominator
  share_hindu_2019     = wm(sh47 %in% 1, hv005),
  share_muslim_2019    = wm(sh47 %in% 2, hv005),
  share_bpl_2019       = { k <- sh75 %in% 0:1; wm(sh75[k] == 1, hv005[k]) },
  share_no_electricity_2019 = wm(hv206 %in% 0, hv005),
  share_q1_2019 = wm(hv270 %in% 1, hv005), share_q2_2019 = wm(hv270 %in% 2, hv005),
  share_q3_2019 = wm(hv270 %in% 3, hv005), share_q4_2019 = wm(hv270 %in% 4, hv005),
  share_q5_2019 = wm(hv270 %in% 5, hv005)
), by = zone_uid]

msg("districts with NFHS-5 data: %d of %d", nrow(ses), nrow(dist))
assert(nrow(ses) >= 630, "too few districts with NFHS-5 data")
fwrite(ses, file.path(dir_derived, "nfhs5_district.csv"))

# ---- check against the frozen app layer -------------------------------------------------
g <- as.data.table(st_drop_geometry(st_read(file.path(dir_data, "golden", "districts.geojson"), quiet = TRUE)))
m <- merge(ses, g[, .(zone_uid, g_w = wealth_mean_2019, g_u = urban_share_2019, g_s = share_scst_2019,
                      g_b = share_bpl_2019, g_m = share_muslim_2019, g_e = share_no_electricity_2019)], by = "zone_uid")
chk <- data.table(variable = c("wealth","urban","scst","bpl","muslim","no_elec"),
  exact = c(m[abs(wealth_mean_2019 - g_w) < 1e-3, .N], m[abs(urban_share_2019 - g_u) < 1e-6, .N],
            m[abs(share_scst_2019 - g_s) < 1e-6, .N], m[abs(share_bpl_2019 - g_b) < 1e-6, .N],
            m[abs(share_muslim_2019 - g_m) < 1e-6, .N], m[abs(share_no_electricity_2019 - g_e) < 1e-6, .N]),
  of = nrow(m), cor = c(cor(m$wealth_mean_2019, m$g_w, use = "c"), cor(m$urban_share_2019, m$g_u, use = "c"),
                        cor(m$share_scst_2019, m$g_s, use = "c"), cor(m$share_bpl_2019, m$g_b, use = "c"),
                        cor(m$share_muslim_2019, m$g_m, use = "c"), cor(m$share_no_electricity_2019, m$g_e, use = "c")))
print(chk)
fwrite(chk, file.path(dir_derived, "check05_vs_frozen.csv"))
if (is_v1) assert(min(chk$exact) >= 630 && min(chk$cor) > 0.99, "NFHS-5 district values no longer reproduce the frozen layer") else
  assert(min(chk$cor) > 0.8, "NFHS-5 district values disagree badly with the frozen layer (min r %.3f)", min(chk$cor))   # v2: clusters re-placed on Census 2011 polygons 10-30 km from the warped ones
