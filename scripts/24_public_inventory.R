# =============================================================================
# 24_public_inventory.R  —  the public, sourced facility table for the website
# -----------------------------------------------------------------------------
# One row per facility of the active inventory (v2, final inventory: 329 rows,
# 233 operating, 60 under construction, 36 announced), exactly as analysed, with:
#   * where the row came from and a source URL (plus a second one where one was
#     found), re-found and checked 30 Sep 2026;
#   * every published capacity figure found, which one the paper uses, what it
#     measures (live IT load vs design / build-out), and a flag where published
#     figures disagree;
#   * the IT capacity the paper actually uses (reported, or modelled from the
#     class weights and the 1,800 MW commercial anchor);
#   * the cross-check against the Atlas of Data Center Politics (NUS
#     Infrastructure Lab, dataset 2026-09-29);
#   * the corrections applied in v2 (with evidence) and the issues still open.
# facility_id = row order of the v1 (submitted) inventory, so ids are stable; ids of rows removed in v2
# (duplicates and non-facilities: data/public/inventory_corrections_v2.csv, *_drop) are not reused.
# Licence: CC BY-NC 4.0 (compilation and annotations; facility facts belong to the cited sources).
# Site coordinates (latitude, longitude: the geocode used to assign the district) are published with location_precision
# (v2: building / campus / locality / city, the precision of the site geocode used to assign the district;
# v1: census_city / dcm_city).
#
# Inputs  the active inventory (00_config path_inventory), outputs[_all]/tab15_facilities.csv,
#         data/public/inventory_sources_20260930.csv, inventory_issues_20260930.csv,
#         inventory_corrections_v2.csv, adp_match_by_row.csv
# Output  outputs/public/india_datacentres_public.csv (+ copy into the app's data/)
# =============================================================================
source(file.path(Sys.getenv("DCEJ_ROOT", unset = (function(){a<-commandArgs(FALSE);f<-sub("^--file=","",grep("^--file=",a,value=TRUE));d<-if(length(f)) dirname(normalizePath(f[1])) else getwd();while(!file.exists(file.path(d,"scripts","00_config.R"))&&d!=dirname(d)) d<-dirname(d);d})()), "scripts", "00_config.R"))
dir_pub <- file.path(dir_data, "public")

inv <- fread(path_inventory)
if (!"facility_id" %in% names(inv)) inv[, facility_id := sprintf("IN%03d", .I)]
assert(nrow(inv) == G(297, V2_N_ALL), "unexpected inventory size %d", nrow(inv))
# district, state and location precision exactly as analysed (01: site geocodes on the Census 2011
# boundaries for v2), joined on facility_id; the MW figures join on dc_id (fixed 3 Oct 2026: the old
# name|operator|lon|lat key broke once 01 replaced the frozen points with site geocodes)
ia <- fread(outp("dc_current_inventory.csv", "all"))[, .(facility_id, dc_id, zone_uid, dist_assigned = dist_name, state_assigned = state_name, geocode_class,
                                                        site_lat = lat, site_lon = lon)]
dl <- as.data.table(st_drop_geometry(read_districts()))[, .(zone_uid, state_layer = state_name)]   # by zone_uid: six district names repeat across states
ia <- merge(ia, dl, by = "zone_uid", all.x = TRUE)
assert(!anyNA(ia$state_layer), "facility zone_uid not in the district layer")
assert(nrow(ia) == nrow(inv) && setequal(ia$facility_id, inv$facility_id), "dc_current_inventory (all) must cover the inventory")
inv <- merge(inv, ia, by = "facility_id")
inv[, k := dc_id]

fac_op  <- fread(outp("tab15_facilities.csv", "operating"))[, .(k = dc_id, mw_it_paper = mw_it, capacity_basis)]
fac_all <- fread(outp("tab15_facilities.csv", "all"))[, .(k = dc_id, mw_it_scenario = mw_it, capacity_basis_all = capacity_basis,
                                                         class_weight_used = class_weight, lambda_used = mw_per_weight_unit)]
src <- fread(file.path(dir_pub, "inventory_sources_20260930.csv"), colClasses = "character")
iss <- fread(file.path(dir_pub, "inventory_issues_20260930.csv"), colClasses = "character")
cor <- fread(file.path(dir_pub, "inventory_corrections_v2.csv"), colClasses = "character")
adp <- fread(file.path(dir_pub, "adp_match_by_row.csv"), colClasses = "character")
assert(all(inv$facility_id %in% src$facility_id), "sources table must cover every facility")

x <- merge(inv, fac_op, by = "k", all.x = TRUE)
x <- merge(x, fac_all, by = "k", all.x = TRUE)
assert(x[status == "operating", all(!is.na(mw_it_paper))] && all(!is.na(x$mw_it_scenario)), "facility MW join failed")
x <- merge(x, src, by = "facility_id", all.x = TRUE)
x <- merge(x, adp, by = "facility_id", all.x = TRUE)
# public form of the retrieval dates: first retrieval and latest check
retrieved_public <- function(v) vapply(v, function(t) {
  d <- sort(unique(regmatches(t, gregexpr("[0-9]{4}-[0-9]{2}-[0-9]{2}", t))[[1]]))
  if (!length(d)) return(t)
  if (length(d) == 1) sprintf("retrieved %s", d) else sprintf("retrieved %s; last checked %s", d[1], d[length(d)])
}, character(1), USE.NAMES = FALSE)
# corrections applied vs issues still open (unverifiable figures, items to verify)
applied <- if (is_v1) cor[0] else cor[field != "_drop"]
# public labels for the corrected fields
applied[field == "lon", field := "longitude"]
applied[field == "lat", field := "latitude"]
applied[field == "dist_name_frozen", field := "district"]
applied[field == "state_name_frozen", field := "state"]
applied[field == "mw_reported", field := "current IT load (MW)"]
applied[field == "mw_reported_buildout", field := "build-out IT load (MW)"]
applied[field == "dc_type", field := "facility type"]
applied <- applied[, .(correction_applied = paste(sprintf("%s: %s -> %s (%s)", field, fifelse(old_value == "", "blank", old_value), new_value, reason), collapse = " | "),
                       correction_evidence = paste(unique(evidence_url[nzchar(evidence_url)]), collapse = " ")), by = facility_id]
if (!"issue_status" %in% names(iss)) iss[, issue_status := "open"]
open_iss <- iss[if (is_v1) TRUE else issue_status == "open",
                .(open_issue = paste(sprintf("[%s] %s", issue_type, issue), collapse = " | ")), by = facility_id]
x <- merge(x, applied, by = "facility_id", all.x = TRUE)
x <- merge(x, open_iss, by = "facility_id", all.x = TRUE)

# ---- archived copies of the sources (25_archive_sources.py; Internet Archive Wayback Machine) --------
arch_f <- file.path(dir_pub, "source_archive.csv")
arch <- if (file.exists(arch_f)) fread(arch_f, colClasses = "character")[method %in% c("existing", "saved", "existing_older", "saved_manual") & nzchar(archive_url)] else data.table(url = character(), archive_url = character())
if (!nrow(arch)) msg("no archived source copies yet: run python3 scripts/25_archive_sources.py, then re-run this script")
arch_of <- function(u) { a <- arch$archive_url[match(u, arch$url)]; fifelse(is.na(a), "", a) }

lab_status <- c(operating = "operating", construction = "under construction", announced = "announced (planned)")
# How the MW figure that matters for the row was set, in words and with its arithmetic: operating rows describe
# the figure used in the paper, planned rows the pipeline-scenario figure. One MW-per-weight-unit factor (lambda,
# from the operating commercial calibration) sizes every facility without a published load.
ANCHOR_MW <- fread(outp("tab15_national.csv", "operating"))[scenario == "central", anchor_mw]
mw_method <- function(basis, type, w, lam, mw) {
  arith <- sprintf("%s weight %s x %.2f MW per weight unit = %.2f MW", type, as.character(w), lam, mw)
  fcase(basis == "reported", "published capacity used directly: IT load as published (tier T1) or derived from a published capacity by a stated rule (tiers T2-T5); the tier and any conversion are given at the start of mw_basis (data/public/CAPACITY_METHOD.md)",
        grepl("^none", basis), "not sized: no published IT load, and the listing is not confirmed as a separate building with its own load (identity screen), so it is assigned 0 MW",
        grepl("derived", basis), paste0("modelled (no published IT load): ", arith,
                                        "; non-commercial facilities take the same MW per weight unit as commercial ones, without rescaling to any total"),
        basis == "anchored", paste0("modelled (no published IT load): ", arith, "; the MW per weight unit is set so that commercial (colocation and hyperscale) capacity sums to the ",
                                    format(ANCHOR_MW, big.mark = ","), " MW national anchor"),
        default = NA_character_)
}
# state_current is the plain current name (the district layer spells three states its own way; state_2015 keeps the layer label)
ST_NAME_FIX <- c("Nct Of Delhi" = "Delhi", "Chattisgarh" = "Chhattisgarh", "Uttarakhaand" = "Uttarakhand")
pub <- x[order(facility_id), .(
  facility_id, facility_name = name, operator, facility_type = dc_type,
  status = lab_status[status], opened_year,
  city, district_2015 = dist_assigned, state_2015 = state_layer, state_current = fifelse(state_assigned %in% names(ST_NAME_FIX), ST_NAME_FIX[state_assigned], state_assigned),
  latitude = site_lat, longitude = site_lon, location_precision = geocode_class,
  inventory_source = c(census = "operator census (operator's own facility list)", dcm = "Data Center Map directory (datacentermap.com)",
                       crosscheck = "operator, company, government or press record (site found during the Atlas of Data Center Politics cross-check; the Atlas is not a source)",
                       government = "government data-centre census (NIC/NICSI, UIDAI, ISRO, CRIS, State Data Centres)")[src],
  mw_it_reported_current = mw_reported, mw_it_reported_buildout = mw_reported_buildout,
  mw_basis, mw_flag, mw_published_figures,
  mw_it_used_in_paper = fifelse(status == "operating", round(mw_it_paper, 3), NA_real_),
  mw_it_used_in_pipeline_scenario = round(mw_it_scenario, 3),
  class_weight = class_weight_used,
  mw_method = fifelse(status == "operating",
                      mw_method(capacity_basis, dc_type, class_weight_used, lambda_used, mw_it_paper),
                      mw_method(capacity_basis_all, dc_type, class_weight_used, lambda_used, mw_it_scenario)),
  source_url, source_url_archive = arch_of(source_url), source_title, source_type,
  source_url_2, source_url_2_archive = arch_of(source_url_2), source_title_2, source_type_2,
  source_verification = verification,
  url_checked = fifelse(url_checked == "fetched", "page read", fifelse(url_checked == "search_result", "search result only", url_checked)),
  status_in_source, status_agrees_with_inventory = status_agrees,
  source_note, sources_retrieved = retrieved_public(retrieved),
  adp_match, adp_campus_name = adp_name, adp_status, adp_stage, adp_size_bucket, adp_source_urls = adp_urls,
  correction_applied, correction_evidence, open_issue,
  inventory_version = "2026-09-30", licence = "CC BY-NC 4.0")]
pub[is.na(adp_match), adp_match := "none"]

# ---- source_note: the public text states what the sources say (figures, dates, disagreements, merged listings,
# author-held copies). The working notes kept in inventory_sources_*.csv (review log) are replaced row by row by
# data/public/source_notes_public.csv (one row per published facility; 14 Nov 2026).
sn_pub <- fread(file.path(dir_pub, "source_notes_public.csv"), encoding = "UTF-8")
assert(setequal(sn_pub$facility_id, pub$facility_id) && !anyDuplicated(sn_pub$facility_id) && all(nzchar(sn_pub$source_note)),
       "source_notes_public.csv must hold exactly one non-empty note per published facility")
pub[sn_pub, source_note := i.source_note, on = "facility_id"]

# ---- source types: one controlled vocabulary (the verification sheets used free-text labels) ----------
# directory | peeringdb | operator | government | press | press release | certification registry | contractor or vendor | other
norm_type <- function(t, u) {
  d <- tolower(sub("^https?://(www\\.|beta\\.|auth\\.|m\\.)?([^/]+).*$", "\\2", u)); t <- tolower(trimws(t))
  out <- fcase(
    !nzchar(u) | is.na(u), "",
    grepl("peeringdb\\.com$", d), "peeringdb",
    grepl("(datacentermap|baxtel|indiamart|datacenterhawk|inflect|ocolo|cloudscene)\\.", d), "directory",
    grepl("uptimeinstitute\\.com$", d), "certification registry",
    grepl("(datacenterdynamics|w\\.media|indianinfrastructure|tele\\.net)\\.", d) | d == "w.media", "press",
    grepl("^(government|parliament)", t), "government",
    grepl("^(operator|company_annual_report)", t) & !grepl("press release", t), "operator",
    grepl("press.?release", t), "press release",
    grepl("^(trade.?press|press|news)", t), "press",
    t %in% c("contractor", "vendor"), "contractor or vendor",
    t %in% c("directory", "trade directory", "trade_directory", "directory_operator_listing", "registry", "baxtel"), "directory",
    default = "other")
  out
}
pub[, `:=`(source_type = norm_type(source_type, source_url), source_type_2 = norm_type(source_type_2, source_url_2))]

# ---- confidence and what we are unsure of (one rating and a plain-language note per row) --------------
# high   = confirmed: documented by a source independent of the Data Center Map directory, plus a second source; no open issue
# low    = directory listing only (or no source), or an open status/existence question
# medium = everything else (confirmed with an open capacity/source/duplicate issue; partial; single_source)
nz0 <- function(v) !is.na(v) & nzchar(v)
assert(pub[source_verification == "confirmed", all(nz0(source_url_2))], "a confirmed row must list a second source (otherwise single_source)")
pub[, confidence := fcase(
  source_verification %in% c("not_found", "directory_only") | (nz0(open_issue) & grepl("\\[(status|existence)\\]", open_issue)), "low",
  source_verification == "confirmed" & !nz0(open_issue), "high",
  default = "medium")]
ver_note <- c(confirmed = "Facility documented by a source independent of the Data Center Map directory, with a second source.",
              directory_only = "Listed in the Data Center Map directory; no source independent of listing sites confirms this facility's current status. Where a second URL is shown it is a listing, a network-registry record or a general company page.",
              single_source = "One independent source only (an operator, government, PeeringDB or press page). Where a second URL is shown it is another page from that same source.",
              partial = "Listed in Data Center Map; the operator confirms a data centre in this city, but not this exact building.",
              not_found = "No current source confirms this facility's reported capacity.")
pub[, confidence_note := paste0(ver_note[source_verification],
  fifelse(status_agrees_with_inventory %in% c("FALSE", "false") & !nz0(correction_applied), paste0(" Source status differs: ", status_in_source, "."), ""),
  fifelse(is.na(mw_it_reported_current) & is.na(mw_it_reported_buildout),
          fifelse(mw_method %like% "^not sized", " No published IT load, and the listing is not confirmed as a separate building with its own load, so no capacity is assigned.",
                  " No published IT load; the capacity used is modelled."), ""),
  fifelse(nz0(mw_flag), paste0(" Capacity: ", mw_flag, "."), ""),
  fifelse(nz0(correction_applied), " Corrected during verification (see correction_applied and correction_evidence).", ""),
  fifelse(nz0(open_issue), paste0(" Open issue: ", open_issue), ""),
  "")]   # the facility's source_note is no longer repeated here (14 Nov 2026)
pub[, confidence_note := trimws(gsub("\\.\\.", ".", confidence_note))]
# ---- how certain the power (MW) figure is, in one column each row -------------------------------------
# the figure that matters: operating rows use the current load; planned rows the build-out (or current)
has_rep <- fifelse(pub$status == "operating", !is.na(pub$mw_it_reported_current),
                   !is.na(pub$mw_it_reported_current) | !is.na(pub$mw_it_reported_buildout))
pub[, mw_certainty := fcase(has_rep & !nz0(mw_flag) & !(nz0(open_issue) & grepl("\\[capacity\\]", open_issue)), "reported IT load",
                            has_rep, "reported, but uncertain (see mw_note)",
                            default = "not published: modelled")]
pub[, mw_note := fifelse(mw_method %like% "^not sized",
  "No IT load is published, and the listing could not be confirmed as a separate building with its own load (identity screen), so it is assigned 0 MW: it counts as a facility but adds no capacity, electricity or emissions.",
  fifelse(
  mw_certainty == "not published: modelled",
    paste0(fifelse(status == "operating", "No current (live) IT load is published for this facility.", "No IT load is published for this facility."), " The MW used is modelled (class weight x ",
           round(fread(outp("tab15_national.csv", "operating"))[scenario == "central", lambda_mw_per_weight], 2),
           " MW per weight unit) and is an allocation, not a measurement.",
           fifelse(status == "operating" & !is.na(mw_it_reported_buildout), paste0(" A build-out of ", mw_it_reported_buildout, " MW IT is published and is used in the pipeline scenario."), ""),
           fifelse(status == "operating" & !is.na(mw_it_reported_buildout) & !is.na(mw_it_used_in_paper) & mw_it_used_in_paper > mw_it_reported_buildout + 1e-6,
                   " The modelled figure exceeds this published build-out: it is a class allocation, not an estimate for this site.", ""),
           fifelse(nz0(mw_published_figures), paste0(" Other figures found (not used: not a live IT load, or its measure is not stated): ", mw_published_figures), "")),
  paste0("Basis: ", fifelse(nz0(mw_basis), mw_basis, "published IT load"), ".",
                   fifelse(nz0(mw_flag), paste0(" Caution: ", mw_flag, "."), ""),
                   fifelse(nz0(open_issue) & grepl("\\[capacity\\]", open_issue), paste0(" Open issue: ", open_issue), ""))))]
pub[, mw_note := trimws(gsub("\\.\\.", ".", mw_note))]
setcolorder(pub, c("facility_id", "facility_name", "operator", "facility_type", "status", "confidence", "confidence_note",
                   "mw_certainty", "mw_note"))

# ---- checks that tie the public table to the analysis ---------------------------------------
assert(pub[status == "operating", .N] == G(209, V2_N_OP) && uniqueN(pub[status == "operating", paste(district_2015, state_2015)]) == G(31, V2_HOST_OP), "operating count / districts")
assert(all(pub$confidence %in% c("high", "medium", "low")) && all(nzchar(pub$confidence_note)), "every row needs a confidence rating and note")
assert(abs(pub[, sum(mw_it_used_in_paper, na.rm = TRUE)] - fread(outp("tab15_national.csv", "operating"))[scenario == "central", total_mw]) < 0.5,
       "paper MW must sum to the central operating total")
assert(pub[!is.na(mw_it_reported_current), all(!is.na(mw_basis) & nzchar(mw_basis))], "every reported MW needs a basis note")
assert(all(grepl("^https?://", pub$source_url)), "every row needs a source URL")
assert(all(is.finite(pub$latitude) & is.finite(pub$longitude)) && pub[, all(latitude > 6 & latitude < 37 & longitude > 68 & longitude < 98)],
       "every row needs site coordinates inside India")
assert(all(nz0(pub$mw_certainty) & nz0(pub$mw_note)), "every row needs a note on how certain its MW figure is")
assert(all(nz0(pub$mw_method)), "every row needs a capacity method")
chk_m <- pub[mw_method %like% "^modelled", .(mw = fifelse(status == "operating", mw_it_used_in_paper, mw_it_used_in_pipeline_scenario), w = class_weight)]
lam <- fread(outp("tab15_national.csv", "operating"))[scenario == "central", lambda_mw_per_weight]
assert(chk_m[, max(abs(mw - w * lam))] < 0.01, "a modelled MW is not class weight x MW per weight unit")
assert(pub[mw_method %like% "^not sized" & status == "operating", all(mw_it_used_in_paper == 0)], "unsized rows must carry 0 MW")

dir.create(file.path(dir_out, "public"), showWarnings = FALSE)
f <- file.path(dir_out, "public", "india_datacentres_public.csv")
fwrite(pub, f)
nz <- function(v) !is.na(v) & nzchar(v)
msg("confidence: %s", pub[, paste(names(table(confidence)), table(confidence), collapse = ", ")])
msg("wrote %s: %d rows (%s); %d with a second source; %d with a capacity flag; %d corrected; %d open issues; %d matched to the Atlas of Data Center Politics",
    f, nrow(pub), INVENTORY, pub[nz(source_url_2), .N], pub[nz(mw_flag), .N], pub[nz(correction_applied), .N], pub[nz(open_issue), .N], pub[adp_match != "none", .N])
# copy into the companion app only when the app's facility layer is from the same inventory
app_data <- file.path(ROOT, "india-datacentre-ej", "data")
if (dir.exists(app_data) && file.exists(file.path(app_data, "facilities.geojson"))) {
  n_app <- nrow(st_read(file.path(app_data, "facilities.geojson"), quiet = TRUE))
  if (n_app == nrow(pub)) { file.copy(f, file.path(app_data, basename(f)), overwrite = TRUE); msg("copied to %s", app_data)
  } else msg("NOT copied to the app: its facilities.geojson has %d rows (rebuild the app data first: python3 scripts/22_build_shiny_data.py)", n_app)
}
