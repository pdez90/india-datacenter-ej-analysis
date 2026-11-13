# Data sources

Every external dataset the analysis uses, with what it is used for, where to obtain it, the version or retrieval
date, and the script that reads it. Citations follow the paper's reference list. None of these files is included
in this repository.

## Facility inventory

| Dataset | Used for | Access | Version / retrieved | Script |
|---|---|---|---|---|
| DataCenterMap directory, India | Base list of facilities | https://www.datacentermap.com/india/ (34 city listings) | Retrieved 31 August 2026; every entry checked against its sources again by the 30 September 2026 cut-off | Inventory construction before 1 Sep 2026 (see README); 00b_inventory_v2.R applies the later corrections |
| Operator facility lists (15 operator groups) | Operator census | Operator websites; every URL is in the public facility table | Retrieved 30 August 2026; checked again by the 30 September 2026 cut-off | Inventory construction before 1 Sep 2026 (see README); 00b_inventory_v2.R |
| Government data-center pages (NIC/NICSI, UIDAI, ISRO, CRIS, state data centres) | Government census | Official pages; every URL is in the public facility table | Compiled 30 September 2026 | 00b_inventory_v2.R |
| Press, filings, rating rationales and other sources cited per facility | Status, capacity and second sources | Every URL and its archived copy is in the public facility table | Up to 30 September 2026 (cut-off) | 24_public_inventory.R |
| Atlas of Data Center Politics, version 1.0 | Cross-check only | https://www.atlasofdatacenterpolitics.com/ | File dated 29 September 2026 | 24_public_inventory.R |
| OpenStreetMap via the Overpass API (`telecom=data_center`, `building=data_center`) | Cross-check only | https://www.openstreetmap.org (Open Database Licence) | Queried 2026 | Inventory construction before 1 Sep 2026 (see README) |
| ATLAS: Global Data Center Map (Ringmast4r) | Assessed as a source; not used to add facilities | https://github.com/Ringmast4r/Global-Data-Center-Map | Retrieved 26 July 2026 (main branch) | Inventory construction before 1 Sep 2026 (see README) |
| PeeringDB facility and exchange-point records | Published facility coordinates; distance to internet exchanges | https://www.peeringdb.com | September 2026 (facilities); 3 October 2026 (exchanges) | 17_siting_robustness.R |
| Esri ArcGIS World Geocoding Service; Nominatim (OpenStreetMap) | Geocoding addresses without published coordinates | https://geocode.arcgis.com; https://nominatim.openstreetmap.org | September 2026 | (geocodes stored in data/facility_geocodes_v2.csv) |
| GeoNames postal-code and cities500 gazetteers | City placeholders; geocoding the corroboration data | https://download.geonames.org | Retrieved 27 July 2026 | Inventory construction before 1 Sep 2026 (see README) |

## Geography and population

| Dataset | Used for | Access | Version | Script |
|---|---|---|---|---|
| Census 2011 district boundaries (DataMeet) | District layer (641 districts) | https://github.com/datameet/maps (Districts/Census_2011; CC BY 2.5 IN), pinned to commit 5ea9037 | Census 2011 | 00a_districts.R |
| NFHS-5 district identifiers | Joining districts to the survey | Distributed with NFHS-5 (DHS Program) | 2019-21 | 00a_districts.R |
| GHSL population and built-up volume, R2023A | Population weights, urban form | Earth Engine `JRC/GHSL/P2023A/GHS_POP/2020`, `JRC/GHSL/P2023A/GHS_BUILT_V/2020` | 2020 | 03_gee_extract.R |
| WorldPop India 2020 | Gridded population (supplementary) | Earth Engine `WorldPop/GP/100m/pop` | 2020 | 03_gee_extract.R |
| GPW v4.11 population count | Population on the InMAP grid | NASA SEDAC, https://doi.org/10.7927/H4JW8BX5 (bundled with the Global InMAP grid) | 2020 | 16c_run_inmap.R, 16d_postprocess_inmap.R |
| UN World Population Prospects 2022 | Adult (25+) population share | https://population.un.org/wpp/ | 2020 | 13_burden.R, 16d_postprocess_inmap.R |

## Social composition

| Dataset | Used for | Access | Version | Script |
|---|---|---|---|---|
| National Family Health Survey (NFHS-5), person recode (IAPR7D) and cluster GPS (IAGE7A) | Caste, religion, wealth, poverty, electricity, urban shares | DHS Program, https://dhsprogram.com (registration required; not redistributable) | 2019-21 | 05_nfhs_district.R |
| Meta Relative Wealth Index, India | Wealth (validation) | https://data.humdata.org/dataset/relative-wealth-index | 2021 release | 04b_env_external.R |

## Environment

| Dataset | Used for | Access | Version / period | Script |
|---|---|---|---|---|
| ACAG surface PM2.5 and uncertainty, V5.GL.06 (van Donkelaar et al., 2021) | Primary PM2.5 | Atmospheric Composition Analysis Group, Washington University in St. Louis | Monthly, 2013-2022 | 04_env_local.R |
| LGHAP v2 PM2.5 (Bai et al., 2024) | Secondary PM2.5 | Zenodo, https://doi.org/10.5281/zenodo.14280024 | Monthly, 2017-2021 | 04b_env_external.R |
| Surface NO2, 1 km (Anenberg et al., 2022) | Primary NO2 | Dataset released with Anenberg et al. (2022), https://doi.org/10.1016/S2542-5196(21)00255-2 | Annual, 2010-2020 | 04_env_local.R |
| Daily MDA8 surface ozone (Wang et al., 2025) | Primary ozone | Zenodo, https://doi.org/10.5281/zenodo.16683952 | Daily, 2013-2022 | 04b_env_external.R |
| Sentinel-5P TROPOMI L3 columns (NO2, SO2, CO, O3, HCHO) | Supplementary air quality | Earth Engine `COPERNICUS/S5P/OFFL/L3_*` | 2019-2024 | 03_gee_extract.R |
| ERA5-Land daily aggregates | Temperature and hot days | Earth Engine `ECMWF/ERA5_LAND/DAILY_AGGR` | 2011-2025 | 03_gee_extract.R |
| MODIS MOD11A2 v061 land-surface temperature | Heat (supplementary) | Earth Engine `MODIS/061/MOD11A2` | 2011-2024 | 03_gee_extract.R |
| VIIRS Day/Night Band annual composites V22 | Nighttime lights | Earth Engine `NOAA/VIIRS/DNB/ANNUAL_V22` | 2019-2024 | 03_gee_extract.R |
| JRC Global Surface Water | Surface-water occurrence | Earth Engine `JRC/GSW1_4/GlobalSurfaceWater` | 1984-2021 | 03_gee_extract.R |
| WRI Aqueduct 4.0 water risk | Baseline water stress | https://www.wri.org/data/aqueduct-global-maps-40-data | Aqueduct 4.0 (2023) | 05e_water.R |

## Electricity system and emissions

| Dataset | Used for | Access | Version | Script |
|---|---|---|---|---|
| WRI Global Power Plant Database v1.3 | Coal and gas plants; plant-level allocation | https://github.com/wri/global-power-plant-database | v1.3 (one plant's coordinates corrected in data/public/plant_corrections.csv) | 04_env_local.R, 16b_build_emissions.R |
| State-level marginal emission factors (Sengupta et al., 2022) | Marginal CO2, SO2, NOx, PM2.5 | KiltHub, https://doi.org/10.1184/R1/19359989 | 2017-18 system | 15a_mef_kilthub.R, 15b_marginal_mef.R |
| CEA CO2 Baseline Database, version 21.0 | Average grid emission factor | https://cea.nic.in/cdm-co2-baseline-database/ | FY2024-25 | 15_attribution.R |
| Prayas Electricity Supply Monitoring Initiative (ESMI) | Supply interruptions for backup diesel | Harvard Dataverse, https://doi.org/10.7910/DVN/CLLZZM | October 2014-December 2019 | 28_esmi_outages.py, 29_backup_diesel.R |
| Diesel generator emission factors | Backup-diesel emissions | CPCB genset norms (2014); U.S. EPA AP-42 Section 3.4 (1996) | - | 29_backup_diesel.R |
| Fuel-specific water-consumption factors | Electricity-related water | Macknick et al. (2012) | - | 15_attribution.R |

## Air-quality model and health

| Dataset | Used for | Access | Version | Script |
|---|---|---|---|---|
| InMAP | Dispersion of attributable emissions | https://github.com/spatialmodel/inmap/releases | v1.9.6 | 16c_run_inmap.R |
| Global InMAP grid and evaluation data (Thakrar et al., 2022) | Variable-resolution grid, meteorology, chemistry | Zenodo, https://zenodo.org/records/6189451 | v1.1.0 | 16c_run_inmap.R |
| GEMM concentration-response function (Burnett et al., 2018) | Attributable deaths | Parameters from Burnett et al. (2018) | - | 16d_postprocess_inmap.R |
| GBD 2023 Results (IHME) | Baseline adult NCD+LRI mortality | https://vizhub.healthdata.org/gbd-results/ | India, 2019-2023 (queried 21 October 2026) | 16d_postprocess_inmap.R |
| Coal-fleet mortality (Cropper et al., 2021) | Proportional mortality check | Published estimate | - | 15_attribution.R |

## Capacity anchors, policy, use and valuation

| Dataset | Used for | Access | Version | Script |
|---|---|---|---|---|
| Savills India, Data Centre Market Watch H1 2026 | 1,800 MW national commercial anchor | https://www.savills.com/research_articles/255800/237969-1 | July 2026 | 15_attribution.R |
| Rubix Data Sciences, India's Data Centre Sector | 1,500 MW lower anchor; 2030 projection | Rubix Industry Insights, July 2026 | July 2026 | 13_burden.R |
| State data-center and IT policies | Policy compilation (18 states) | Every notification, URL, quote and retrieval date is in data/state_dc_policies_sources.csv | Retrieved 2026 | 14_policy_table.R |
| Anthropic Economic Index, India brief | State shares of Claude.ai use | https://www.anthropic.com/research/india-brief-economic-index | 16 February 2026 | 21_benefit_burden.R |
| Value of a statistical life | Monetized ledger | Viscusi and Masterman (2017); Majumder and Madheswaran (2018) | 2020 US$ | 23_monetised_ledger.R |
| Social cost of carbon | Monetized ledger | Ricke et al. (2018); U.S. EPA (2023) | 2020 US$ | 23_monetised_ledger.R |

## The authors' own input tables

These tables were compiled by the authors. They live in `data/` and are not in this repository. "Companion
repository" means https://github.com/pdez90/india-datacentre-ej. The others are available from the authors on
request; everything they contain that bears on a facility (sources, corrections, statuses, capacities, coordinates
with precision class) is reported row by row in the public facility table.

| File | Contents | Released | Script |
|---|---|---|---|
| `data/inventory_frozen_20260901.csv` | The inventory as it stood on 1 September 2026 (see README) | On request | 00b_inventory_v2.R |
| `data/public/inventory_corrections_v2.csv` | 341 documented corrections (29 removals, 312 field edits), each with reason and evidence URL | Reported in the public table (`correction_applied`, `correction_evidence`); file on request | 00b_inventory_v2.R, 24_public_inventory.R |
| `data/public/inventory_additions_v2.csv` | Facilities added after 1 September 2026, with sources | Rows are in the public table; file on request | 00b_inventory_v2.R |
| `data/facility_geocodes_v2.csv` | Site geocode, address, method and precision for each facility | Coordinates and precision class are in the public table; file on request | 01_inventory.R |
| `data/public/facility_district_changes_v2.csv` | District reassignments from the site geocodes | On request | 01_inventory.R |
| `data/public/inventory_sources_20260930.csv`, `inventory_issues_20260930.csv` | Source records and open issues per facility | Reported in the public table; files on request | 24_public_inventory.R |
| `data/public/adp_match_by_row.csv` | Match to the Atlas of Data Center Politics for each facility | In the public table (`adp_*` columns) | 24_public_inventory.R |
| `data/public/unresolved_facilities_20261025.csv` | The 75 facilities that public sources neither confirm nor refute | Companion repository (`data/unresolved_facilities.csv`, with current status and MW) | 16b_build_emissions.R (unresolved_zero run) |
| `data/public/plant_corrections.csv` | Corrected coordinates of one GPPD plant (Solapur) | On request; the correction is stated in SI section S12 | 04_env_local.R, 16b_build_emissions.R |
| `data/india_grid_params.csv` | Capacity anchors, PUE, utilization, WUE and grid water factors (low, central, high) | Values are in SI Table S4.2; file on request | 13, 15, 15c, 32 |
| `data/state_dc_policies.csv`, `data/state_dc_policies_sources.csv` | State policy compilation and its sources | Companion repository (`data/policies.csv`, `data/policy_sources.csv`) | 14_policy_table.R |
| `data/sengupta_mef_flatload.csv`, `sengupta_mef_long.csv`, `sengupta_mef_flatload_tableS43.csv` | Marginal emission factors derived from Sengupta et al. (2022) | Rebuilt by 15a_mef_kilthub.R from the KiltHub files; Table S4.3 in the SI | 15a, 15b, 32 |
| `data/ixp_india_peeringdb_20261003.csv` | The 42 internet exchange points in 13 cities used for the connectivity models (from PeeringDB, 3 October 2026) | On request; PeeringDB is public | 17_siting_robustness.R |
| `data/golden/*.geojson` | The authors' frozen reference layers from the 1 September 2026 pipeline | Not released; the comparisons that read them are skipped when absent | 03, 04, 04b, 05, 05e, 06 (v1 only), 15 and 16b (v1 only), 16d, 16e, 20b, 21 |
