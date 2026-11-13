# India data-center environmental justice: analysis code

Code for *The Environmental Geography of Data Centers in India* (de Souza et al.). It builds the facility
inventory of 30 September 2026, links it to district-level environmental and social data, attributes electricity
demand and emissions to the sector, disperses the emissions with Global InMAP, and produces every table and figure
in the paper and Supplementary Information.

This repository holds the code only. The raw data are not redistributed: several are large (about 24 GB in all)
and the NFHS-5 survey microdata may not be redistributed under the DHS terms of use. `DATA_SOURCES.md` lists every
dataset, where to obtain it, and which script reads it.

The code that first assembled the inventory (directory download, operator census, cross-checks) was lost on
29 September 2026. The pipeline here starts from the inventory as it stood on 1 September 2026
(`data/inventory_frozen_20260901.csv`) and applies every later addition and correction from documented, sourced
tables (`00b_inventory_v2.R`), which produces the inventory of 30 September 2026 used in the paper.

The public facility table (329 facilities, CC BY-NC 4.0, with sources, archived copies, verification status,
confidence, capacity basis and corrections) and the companion app are at
https://github.com/pdez90/india-datacentre-ej.

## Layout

```
scripts/         R and Python scripts, run in numeric order by scripts/run_all.R
docs/            CAPACITY_METHOD.md: how each facility's IT capacity is set (tiers T1-T6)
DATA_SOURCES.md  every external dataset, with access details
```

The scripts expect this folder layout around them (created on first run, not tracked in git):

```
data/            inputs (see DATA_SOURCES.md); data/public holds the inventory build files
outputs/         results for the operating inventory (DC_SCOPE=operating, the primary analysis)
outputs_all/     results for the stock-plus-pipeline scenario (DC_SCOPE=all)
inmap_work/      InMAP emissions, configuration and output
paper/figs/      publication figures
tools/           the InMAP binary
```

## Requirements

- R (4.3 or later) with data.table, sf, terra, exactextractr, ncdf4, ggplot2, haven, jsonlite, and rgee for the
  Earth Engine step.
- Python 3 with numpy, pandas and matplotlib.
- Global InMAP v1.9.6 (https://github.com/spatialmodel/inmap/releases), placed in `tools/`.
- A Google Earth Engine account for `03_gee_extract.R` (set `GEE_PROJECT` to your Cloud project).

Paths to the large external files are set in `scripts/00_config.R` and can be overridden with environment
variables (`DCEJ_NFHS5_PR`, `DCEJ_NFHS5_GPS`, `DCEJ_ACAG_PM`, `DCEJ_ACAG_SIGMA`, `DCEJ_NO2_1KM`, `DCEJ_EXT_DIR`,
`DCEJ_INMAP_BIN`).

## Running

```
Rscript scripts/run_all.R --skip-gee                # whole pipeline (Global InMAP takes about 6 h per scope)
Rscript scripts/run_all.R --skip-gee --skip-inmap   # reuse existing InMAP fields
Rscript scripts/run_all.R --from 16d --until 23     # a range of steps
Rscript scripts/run_sensitivity.R                   # the three InMAP sensitivity runs (SI Table S4.1.3)
Rscript scripts/check_pipeline.R                    # checks every published value against the outputs
```

Steps not run by `run_all.R` (network-bound): `03_gee_extract.R` (Earth Engine), `25_archive_sources.py`,
`26_check_links.py`, `26_local_snapshots.py` and `27_snapshot_sources.py` (source archiving and link checks).

## Contact

Priyanka deSouza, University of Colorado Denver.
