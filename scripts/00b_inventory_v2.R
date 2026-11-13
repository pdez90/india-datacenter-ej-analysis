# =============================================================================
# 00b_inventory_v2.R  —  the revised inventory (v2, 30 Sep 2026)
# -----------------------------------------------------------------------------
# v2 = v1 (data/inventory_frozen_20260901.csv, the submitted paper's inventory)
# plus the explicit, sourced edits in data/public/inventory_corrections_v2.csv,
# found by re-finding every facility's sources on 30 Sep 2026 (see DATA.md and
# the public table). Every edit names the facility, the field, the old and the
# new value, the reason and the evidence URL; nothing else changes.
#   - 3 duplicate rows removed (IN251 STT Pune DC 1 directory copy; IN281 Tata
#     VSB inactive listing at the STT Delhi 1 site; IN139 second MAA10 listing)
#   - status: IN174 Nxtra Agartala I and IN262 Sify Hyderabad 02 -> construction;
#     IN233 STT Jaipur -> operating (6 MW IT)
#   - IN157 NTT Chennai 2: live 17.4 MW (building B is row IN110)
#   - IN067 renamed: the 72 MW is the whole CtrlS Chennai park (DC1 + DC2)
#   - build-out: IN003 AdaniConneX Chennai 1 33 MW; IN267 Sify Rabale 240 MW
#   - 8 Kochi rows: district Alappuzha -> Ernakulam (01 honours dist_name_frozen
#     as an explicit override where the point falls in no polygon)
# Batch 2 (30 Sep 2026, second pass; `batch` column = 2), after a cross-check against the Atlas of
# Data Center Politics and a re-check of every operator's own facility list:
#   - 12 more duplicates removed: the 11 Tata Communications listings are STT GDC buildings (Tata sold
#     its data-centre business to STT in 2016) and IN191 repeats IN189 (Nxtra PUN 3)
#   - statuses from dated evidence (AdaniConneX Hyderabad live; AWS Meerkhanpet under construction;
#     Ashok Panchkula live; RackBank Chennai and RailTel Noida not yet built; Nxtra, STT updates)
#   - published IT loads from operator pages (Nxtra, STT, NTT, Sify, Yotta, CtrlS)
#   - 3 colocation types; 4 district fixes where the row sat in the wrong city (Manesar x2, Mohali, Ludhiana)
# Additions (data/public/inventory_additions_v2.csv, IN298-IN360): facilities verified against primary
# sources, found through the Atlas cross-check (src = atlas) or the government data-centre census
# (src = government: NIC/NICSI national data centres, UIDAI, ISRO, CRIS and the State Data Centres).
# Closed sites are excluded; candidates reviewed but not added are listed with reasons in
# data/public/inventory_candidates_not_added_v2.csv.
# facility_id (IN001-IN297 = row order of v1; IN298+ = additions) is carried so ids stay stable.
# =============================================================================
source(file.path(Sys.getenv("DCEJ_ROOT", unset = (function(){a<-commandArgs(FALSE);f<-sub("^--file=","",grep("^--file=",a,value=TRUE));d<-if(length(f)) dirname(normalizePath(f[1])) else getwd();while(!file.exists(file.path(d,"scripts","00_config.R"))&&d!=dirname(d)) d<-dirname(d);d})()), "scripts", "00_config.R"))

v1 <- fread(file.path(dir_data, "inventory_frozen_20260901.csv"), colClasses = list(character = c("name")))
assert(nrow(v1) == 297, "v1 must have 297 rows")
v1[, facility_id := sprintf("IN%03d", .I)]
ed <- fread(file.path(dir_data, "public", "inventory_corrections_v2.csv"), colClasses = "character")

v2 <- copy(v1)
add <- fread(file.path(dir_data, "public", "inventory_additions_v2.csv"), colClasses = list(character = c("facility_id", "name", "opened_year")))
add[, geocode := NULL]
assert(!any(add$facility_id %in% v1$facility_id) && !anyDuplicated(add$facility_id), "addition ids must be new and unique")
assert(setequal(names(add), names(v2)), "additions must have the inventory columns")
add[, opened_year := as.integer(opened_year)]
v2 <- rbind(v2, add[, names(v2), with = FALSE])   # additions first, so corrections can also apply to them
assert(all(ed$facility_id %in% v2$facility_id), "unknown facility_id in the corrections file")
for (i in seq_len(nrow(ed))) {
  e <- ed[i]; r <- which(v2$facility_id == e$facility_id)
  if (e$field == "_drop") next
  assert(e$field %in% names(v2), "unknown field %s", e$field)
  old <- v2[[e$field]][r]
  same <- if (is.numeric(old)) (is.na(old) && e$old_value == "") || isTRUE(abs(old - as.numeric(e$old_value)) < 1e-9) else identical(as.character(old), e$old_value)
  assert(same, "%s %s: expected old value '%s', found '%s'", e$facility_id, e$field, e$old_value, as.character(old))
  set(v2, r, e$field, if (is.numeric(old)) as.numeric(e$new_value) else e$new_value)
}
v2 <- v2[!facility_id %in% ed[field == "_drop", facility_id]]
setcolorder(v2, "facility_id")
fwrite(v2, file.path(dir_data, "inventory_v2_20260930.csv"))
msg("v2 inventory: %d rows (%d operating, %d construction, %d announced); %d edits applied, %d rows removed, %d added",
    nrow(v2), v2[status == "operating", .N], v2[status == "construction", .N], v2[status == "announced", .N],
    nrow(ed[field != "_drop"]), nrow(ed[field == "_drop"]), nrow(add))
assert(nrow(v2) == V2_N_ALL && v2[status == "operating", .N] == V2_N_OP, "v2 must be %d rows / %d operating", V2_N_ALL, V2_N_OP)
