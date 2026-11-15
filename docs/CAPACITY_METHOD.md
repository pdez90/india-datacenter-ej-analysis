# How each facility's IT capacity is set

Inventory snapshot: 30 September 2026. Method fixed 26 October 2026 and applied to every row. The tier used for a row is
named at the start of its `mw_basis` field in `inventory_sources_20260930.csv` and appears in the public table's `mw_note`.
Every figure carries its source URL and, where the source is dated, its date, in `mw_published_figures`.

## Which sources count

Operator pages and fact sheets, exchange filings, annual reports, earnings transcripts and investor presentations, rating
rationales, government pages and tenders, and dated reputable trade press (DCD, W.Media, Economic Times, Business Standard,
The Hindu BusinessLine, Inc42 and similar) are admissible. Directory and listing sites (Data Center Map, Baxtel, Ocolo,
Datacenterhawk, Cloudscene, IndiaMART, JustDial, LinkedIn company pages) are not: they may copy one another and do not
state a basis. A figure that appears only on a listing site is never used for capacity (CtrlS Mumbai DC1/DC2, batch 7).

## Precedence when sources disagree

1. A figure for this facility beats a figure for the operator as a whole or for a campus the facility shares.
2. A dated source on or before 30 September 2026 beats an undated page. A later dated source beats an earlier one.
   Evidence dated after the cutoff cannot change the snapshot unless it states a date before the cutoff.
3. Among undated operator pages, the facility fact sheet or location card beats running prose (STT Ahmedabad, Kolkata).
4. Operator and filing beat press; press beats nothing else admissible.
5. Where admissible sources still conflict and none supersedes, the row keeps the figure already in the inventory and the
   conflict is recorded in `mw_flag` (Nxtra Chennai 47 vs 46; CtrlS Bangalore 12 vs 13; Yotta D1 30 vs 40).

## The tiers (applied in this order; the first tier with an admissible figure is used)

| Tier | Published | Rule | Flag text in `mw_flag` |
|---|---|---|---|
| T1 | Live IT load for this facility, labelled IT / critical IT / IT power | Used as published | none |
| T2 | Live "operational capacity", "capacity" or MVA for this facility, basis not stated, from operator, filing, government or rating agency | Used as IT load; 1 MVA = 1 MW | "operational capacity ... basis not stated ... used as IT load (rule T2)" |
| T3 | Total / facility / utility power for this facility only | IT = total power / 1.6 (the paper's PUE, so the conversion is the inverse of the one used to turn IT MW into electricity) | "derived from published total power at PUE 1.6" |
| T4 | A company-wide or campus-wide figure with no split by facility | Facilities with their own figure (T1–T3) keep it; the remainder is apportioned equally across the operator's other operating facilities | "apportioned from a company-wide / campus figure" |
| T5 | Rack count only (government and NIC data centres) | IT = racks x 4 kW. Installed or commissioned racks are used; a design or total rack capacity is used only when no installed count is published, and the flag says so | "derived from rack count at 4 kW per rack (installed / design)" |
| T6 | Nothing admissible | Class-weight imputation: colocation 3, hyperscale 10, enterprise 1, government 1, telecom 0.5, times lambda; lambda is set so that operating commercial (colocation + hyperscale) capacity sums to the 1,800 MW national anchor; non-commercial facilities take the same lambda without rescaling | `mw_certainty` = "not published: modelled" |
| — | Listing with no evidence of a distinct building with its own load (identity screen) | 0 MW | `mw_method` = "not sized" |

Note under T1/T2: operators often publish one "IT load capacity" for an operating facility without saying whether it is the live or the design figure. For an operating single-phase facility that figure is used as current (and as build-out); where the source itself calls it a design figure the flag says "design IT load used as current" (CtrlS Hyderabad DC3). Where a figure is called "power capacity" or "capacity" with no qualifier it is T2; it is treated as total power (T3) only when a second admissible source labels the same figure total/utility power (CtrlS Mumbai DC1+DC2, where PeeringDB gives 42 MW total power) or the operator's own IT figure for the site is far below it (Silvernox Navi Mumbai).

Build-out (`mw_reported_buildout`) follows the same tiers for the facility's full design IT load; "potential", master-plan
and campus-ambition figures are recorded in `mw_published_figures` but not used.

## Facility type

A row is `colocation` when the operator publishes multi-tenant colocation capacity or sells space in that building to
third parties at facility scale; `hyperscale` when the building is a cloud provider's own; `enterprise` for a company's
own facility, including small hosting and ISP firms whose only evidence of a facility is an office-scale site or a
directory listing (their marketing of "colocation" does not by itself make the building a colocation data centre; the
enterprise weight keeps the imputed load conservative); `government` for state, NIC and central-agency data centres;
`telecom` for operators' network facilities.

## Status

`operating` requires dated evidence of go-live, inauguration or operational capacity on or before the cutoff. A
groundbreaking, foundation stone, target date or "scheduled" leaves a row at `construction` if works have begun or
`announced` if not. A phased project with a live first phase is `operating`, with the live phase as current capacity and
the design as build-out.

## What the tiers do not settle

T2–T5 are documented approximations, not measurements: T2 may include non-IT load; T3 and T5 depend on the PUE and
rack-density assumptions stated above; T4 assumes equal sites. The public table marks every T2–T5 row
"reported, but uncertain (see mw_note)". Seventy-five rows could not be confirmed or refuted from public sources and carry
an open issue naming the evidence that would settle them (`data/public/unresolved_facilities_20261025.csv`); the
`unresolved_zero` sensitivity run removes their load.
