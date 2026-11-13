#!/usr/bin/env python3
"""22_build_shiny_data.py -- bake the companion app's data (india-datacentre-ej/data/) from the
v2 pipeline outputs of BOTH scopes.

Replaces the lost scripts/22_build_shiny_data.R (2 Sep 2026, v1 inventory). Standard library only.
Run from the project root after both scopes are complete (16d, 15, 15b, 14, 20a, 20b, 24, 28, 29 and 30):
    python3 scripts/22_build_shiny_data.py

Reads
  outputs/      operating scope, INVENTORY=v2   (dc_current_inventory, tab15*, tab16*, tab14)
  outputs_all/  stock-plus-pipeline scope, v2
  data/derived/analysis_district.csv           district attributes (06)
  data/derived/districts_census2011.geojson    district polygons (geometry only; 00a_districts.R)
  data/golden/osm_check.geojson                OpenStreetMap cross-check (independent of the inventory)
  data/golden/headline.json                    context constants only
  data/state_dc_policies_sources.csv           claim-level policy sources
  outputs/public/india_datacentres_public.csv  sourced facility table (24)
Writes india-datacentre-ej/data/: districts.geojson, facilities.geojson, plants.geojson, osm_check.geojson,
  policies.csv, policy_sources.csv, headline.json, ej_panel.json, india_datacentres_public.csv,
  india_datacentres_documented.zip, district_power_interruptions.csv, esmi_monitor_power_interruptions.csv
Also reads outputs/tab28_esmi_district_outages.csv and data/derived/esmi_monitor_outages.csv (scripts/28).

Backup diesel (29_backup_diesel.R): district diesel NOx, central scenario (diesel_nox_t), and the diesel shares
quoted in the text (tab29_diesel_summary.txt / tab29_diesel_scenarios.csv).
"""
import csv, io, json, os, re, shutil, sys, zipfile
from collections import defaultdict
from datetime import date

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
OP, ALL = os.path.join(ROOT, 'outputs'), os.path.join(ROOT, 'outputs_all')
APP = os.path.join(ROOT, 'india-datacentre-ej', 'data')
GOLD = os.path.join(ROOT, 'data', 'golden')

def rd(*p): return list(csv.DictReader(open(os.path.join(*p), encoding='utf-8')))
def num(x):
    if x is None: return None
    x = str(x).strip()
    if x in ('', 'NA', 'NaN', 'nan', 'Inf', '-Inf'): return None
    return float(x)
def r4(x): return None if x is None else round(x, 4)
def tf(x): return str(x).strip().upper() == 'TRUE'
def sorted_median(v):
    v = sorted(x for x in v if x is not None); n = len(v)
    return None if not n else (v[n // 2] if n % 2 else (v[n // 2 - 1] + v[n // 2]) / 2)
def fail(msg): sys.exit('22: CHECK FAILED - ' + msg)
def check(ok, msg):
    if not ok: fail(msg)

# ---- the two scopes must both be v2 ---------------------------------------------------------
summ = {}
for sc, d in (('operating', OP), ('all', ALL)):
    t = open(os.path.join(d, 'tab16_inmap_summary.txt'), encoding='utf-8').read()
    check(f'DC_SCOPE={sc}, INVENTORY=v2' in t, f'{d}/tab16_inmap_summary.txt is not DC_SCOPE={sc}, INVENTORY=v2')
    g = lambda pat: float(re.search(pat, t).group(1))
    summ[sc] = dict(text=t,
        pwm_ugm3=g(r'increment \(grid\): ([\d.]+)'), pwm_district_ugm3=g(r'district-mean equivalent, for comparison:\s+([\d.]+)'),
        pop_ge_0_01_million=int(g(r'>= 0\.01 ug/m3 \(grid cells\): (\d+)')), pop_ge_0_10_million=int(g(r'>= 0\.10 ug/m3 \(grid cells\): (\d+)')),
        deaths_per_yr=int(g(r'deaths from the increment: (\d+)')))

# ---- facilities -----------------------------------------------------------------------------
inv = {r['dc_id']: r for r in rd(ALL, 'dc_current_inventory.csv')}
f_all = {r['dc_id']: r for r in rd(ALL, 'tab15_facilities.csv')}
f_op = {r['dc_id']: r for r in rd(OP, 'tab15_facilities.csv')}
check(len(inv) == len(f_all) and set(inv) == set(f_all), 'all-scope inventory and tab15 disagree')
check(set(f_op) <= set(inv), 'operating tab15 has facilities not in the all-scope inventory')
check({k for k, r in inv.items() if r['status'] == 'operating'} == set(f_op), 'operating set differs between scopes')

ad = {r['zone_uid']: r for r in rd(ROOT, 'data', 'derived', 'analysis_district.csv')}
# district geometry: the Census 2011 boundaries with NFHS identifiers (00a_districts.R; v2, 641 districts).
# The old app layer (data/golden/districts.geojson) used the warped NFHS polygons and is no longer used.
DIST_GJ = os.path.join(ROOT, 'data', 'derived', 'districts_census2011.geojson')
check(os.path.exists(DIST_GJ), 'run scripts/00a_districts.R first (district layer)')
gold_d = json.load(open(DIST_GJ, encoding='utf-8'))
N_DIST = len(gold_d['features'])
check(len(ad) == N_DIST, f'analysis_district.csv must have {N_DIST} districts')

feats = []
for k in sorted(inv, key=lambda k: inv[k]['facility_id']):
    i, a, o = inv[k], f_all[k], f_op.get(k)
    z = ad.get(i['zone_uid']); check(z is not None, f'{k}: zone_uid {i["zone_uid"]} not in the district layer')
    lon, lat = num(i['lon']), num(i['lat']); check(lon is not None and lat is not None, f'{k}: no coordinates')
    # exact site geocodes are published (public table latitude/longitude), so the app draws them unrounded
    p = dict(facility_id=i['facility_id'], name=i['name'], operator=i['operator'], status=i['status'], src=i['src'],
             opened_year=None if num(i['opened_year']) is None else int(num(i['opened_year'])),
             mw_reported=num(i['mw_reported']), mw_reported_buildout=num(i['mw_reported_buildout']),
             dist_name=z['dist_name'], state_name=z['state_name_2015'], dc_type=i['dc_type'],
             mw_it_all=r4(num(a['mw_it'])), e_fac_gwh_all=r4(num(a['e_fac_gwh'])), co2_kt_all=r4(num(a['co2_kt'])),
             scope1_ml_all=r4(num(a['scope1_ml'])), scope2_ml_all=r4(num(a['scope2_ml'])),
             mw_it=r4(num(o['mw_it'])) if o else None, e_fac_gwh=r4(num(o['e_fac_gwh'])) if o else None,
             co2_kt=r4(num(o['co2_kt'])) if o else None, scope1_ml=r4(num(o['scope1_ml'])) if o else None,
             scope2_ml=r4(num(o['scope2_ml'])) if o else None, is_operating=i['status'] == 'operating')
    feats.append(dict(type='Feature', properties=p, geometry=dict(type='Point', coordinates=[lon, lat])))

# ---- districts ------------------------------------------------------------------------------
dp_op = {r['zone_uid']: num(r['dpm25']) for r in rd(OP, 'tab16_inmap_district.csv')}
dp_all = {r['zone_uid']: num(r['dpm25']) for r in rd(ALL, 'tab16_inmap_district.csv')}
dd_op = {r['zone_uid']: num(r['deaths']) for r in rd(OP, 'tab16_inmap_district.csv')}      # GEMM attributable deaths per district
dd_all = {r['zone_uid']: num(r['deaths']) for r in rd(ALL, 'tab16_inmap_district.csv')}
s1, s2 = defaultdict(float), defaultdict(float)
for r in f_op.values():
    s1[r['zone_uid']] += num(r['scope1_ml']) or 0; s2[r['zone_uid']] += num(r['scope2_ml']) or 0
pts = rd(OP, 'tab16b_emissions_points.csv')
em = defaultdict(lambda: [0.0, 0.0, 0.0])
for r in pts:
    e = em[r['zone_uid']]; e[0] += num(r['so2_t']) or 0; e[1] += num(r['nox_t']) or 0; e[2] += num(r['pm25_t']) or 0

DIESEL_TAB = os.path.join(OP, 'tab29_diesel_district.csv')
check(os.path.exists(DIESEL_TAB), 'run scripts/29_backup_diesel.R first (backup diesel)')
diesel = {r['zone_uid']: num(r['diesel_nox_t_central']) for r in rd(DIESEL_TAB)}
dsc = [r for r in rd(OP, 'tab29_diesel_scenarios.csv')]
dcen = [r for r in dsc if abs(num(r['f']) - 0.4) < 1e-9 and abs(num(r['testing_h']) - 100) < 1e-9][0]
ESMI_TAB = os.path.join(OP, 'tab28_esmi_district_outages.csv')
check(os.path.exists(ESMI_TAB), 'run scripts/28_esmi_outages.py first (district power-interruption hours)')
esmi_rows = rd(ESMI_TAB); esmi = {r['zone_uid']: r for r in esmi_rows}
esmi_mon = rd(ROOT, 'data', 'derived', 'esmi_monitor_outages.csv')
KEEP = ['dc_count', 'dc_count_all', 'pop_2020', 'wealth_mean_2019', 'rwi_mean', 'urban_share_2019', 'share_scst_2019',
        'share_bpl_2019', 'share_muslim_2019', 'share_no_electricity_2019', 'pm25_acag_local', 'no2_surf', 'o3_summer',
        'hot_days_peryr', 'coal_mw', 'dist_fossil_km', 'bws_raw', 'wealth_pct']
dfeats = []
for g in gold_d['features']:
    z = g['properties']['zone_uid']; a = ad[z]
    p = dict(zone_uid=z, dist_name=a['dist_name'], state_name=a['state_name_2015'])
    for c in KEEP:
        v = num(a[c]); p[c] = (int(v) if c.startswith('dc_count') else r4(v)) if v is not None else None
    p['dpm25'] = r4(dp_op.get(z)); p['dpm25_all'] = r4(dp_all.get(z))
    p['deaths'] = r4(dd_op.get(z)); p['deaths_all'] = r4(dd_all.get(z))
    p['s1_water_ml'] = round(s1[z], 3); p['s2_water_ml'] = round(s2[z], 3)
    e = em.get(z, [0, 0, 0]); p['emis_so2_t'], p['emis_nox_t'], p['emis_pm25_t'] = (round(x, 3) for x in e)
    o = esmi.get(z)   # Prayas ESMI supply-interruption hours (scripts/28); None where no monitor
    p['esmi_h_yr'] = num(o['interruption_h_per_yr']) if o else None
    p['esmi_h_yr_urban'] = num(o['interruption_h_per_yr_urban']) if o else None
    p['esmi_n_monitors'] = int(o['n_monitors']) if o else 0
    p['diesel_nox_t'] = round(diesel.get(z, 0.0), 3)
    dfeats.append(dict(type='Feature', properties=p, geometry=g['geometry']))
check(len(dfeats) == N_DIST, f'district layer must have {N_DIST} features')

# per-district counts must equal the facility points
cnt_op, cnt_all = defaultdict(int), defaultdict(int)
for f in feats:
    key = (f['properties']['dist_name'], f['properties']['state_name']); cnt_all[key] += 1
    if f['properties']['is_operating']: cnt_op[key] += 1
for d in dfeats:
    key = (d['properties']['dist_name'], d['properties']['state_name'])
    check(d['properties']['dc_count'] == cnt_op[key] and d['properties']['dc_count_all'] == cnt_all[key],
          f'district counts != facility points in {key}')

# ---- plants (operating attributable tonnes; the all-scope tonnes are added as *_all) ----------
pts_all = {r['gppd_idnr']: r for r in rd(ALL, 'tab16b_emissions_points.csv')}
pfeats = []
for r in pts:
    q = pts_all.get(r['gppd_idnr'], {})
    p = dict(name=r['name'], fuel=r['fuel'], state=r['state'], capacity_mw=num(r['capacity_mw']),
             so2_t=r4(num(r['so2_t'])), nox_t=r4(num(r['nox_t'])), pm25_t=r4(num(r['pm25_t'])),
             so2_t_all=r4(num(q.get('so2_t'))), nox_t_all=r4(num(q.get('nox_t'))), pm25_t_all=r4(num(q.get('pm25_t'))))
    pfeats.append(dict(type='Feature', properties=p, geometry=dict(type='Point', coordinates=[num(r['lon']), num(r['lat'])])))

# ---- policies -------------------------------------------------------------------------------
pol = rd(OP, 'tab14_policy_burden.csv'); pol_all = {r['state']: r for r in rd(ALL, 'tab14_policy_burden.csv')}
pcols = list(pol[0].keys()) + ['dc_count_all']
for r in pol: r['dc_count_all'] = pol_all[r['state']]['dc_count']
psrc = rd(ROOT, 'data', 'state_dc_policies_sources.csv')

# ---- headline -------------------------------------------------------------------------------
nat = {r['scenario']: r for r in rd(OP, 'tab15_national.csv')}['central']
nat_all = {r['scenario']: r for r in rd(ALL, 'tab15_national.csv')}['central']
mt_op, mt_all = rd(OP, 'tab15b_marginal_totals.csv')[0], rd(ALL, 'tab15b_marginal_totals.csv')[0]
gold_h = json.load(open(os.path.join(GOLD, 'headline.json'), encoding='utf-8'))
fp = [f['properties'] for f in feats]
has = [r for r in pol if tf(r['has_policy'])]
# The app quotes the district-mean population-weighted increment (the paper's figure); the grid value is kept for reference.
inmap = lambda sc: dict(pwm_ugm3=summ[sc]['pwm_district_ugm3'], pwm_grid_ugm3=summ[sc]['pwm_ugm3'],
                        **{k: summ[sc][k] for k in ('pop_ge_0_01_million', 'pop_ge_0_10_million', 'deaths_per_yr')})
H = dict(
    built=date.today().isoformat(), inventory='revised 30 Sep 2026',
    n_operating=sum(p['is_operating'] for p in fp), n_all=len(fp),
    n_construction=sum(p['status'] == 'construction' for p in fp), n_announced=sum(p['status'] == 'announced' for p in fp),
    hosting_districts_operating=sum(d['properties']['dc_count'] > 0 for d in dfeats),
    hosting_districts_all=sum(d['properties']['dc_count_all'] > 0 for d in dfeats),
    mw_operating=round(num(nat['total_mw'])), mw_all=round(num(nat_all['total_mw'])),
    anchor_commercial_mw=round(num(nat['anchor_mw'])),
    twh_operating=round(num(nat['e_fac']), 1), twh_all=round(num(nat_all['e_fac']), 1),
    pct_nat_gen_operating=round(num(nat['pct_national_generation']), 1),
    co2_mt_avg_operating=round(num(nat['co2_mt']), 1),
    co2_mt_marginal_operating=round(num(mt_op['co2_mt']), 1), co2_mt_marginal_all=round(num(mt_all['co2_mt']), 1),
    scope1_gl_operating=round(num(nat['scope1_gl']), 1), scope2_gl_operating=round(num(nat['scope2_gl']), 1),
    scope2_gl_nohydro_operating=round(num(nat['scope2_gl_nohydro']), 1),
    so2_kt_operating=round(sum(num(r['so2_t']) for r in pts) / 1000, 1),
    nox_kt_operating=round(sum(num(r['nox_t']) for r in pts) / 1000, 1),
    pm25_kt_operating=round(sum(num(r['pm25_t']) for r in pts) / 1000, 1),
    diesel_nox_pct_central=round(num(dcen['pct_grid_nox']), 1),
    diesel_nox_pct_range='%.1f-%.1f' % (min(num(r['pct_grid_nox']) for r in dsc), max(num(r['pct_grid_nox']) for r in dsc)),
    diesel_nox_t_central=round(num(dcen['diesel_nox_t'])),
    policy_states_dedicated=len(has), policy_states_reviewed=len(pol),
    facilities_in_policy_states=sum(int(r['dc_count']) for r in has),
    facilities_in_policy_states_all=sum(int(r['dc_count_all']) for r in has),
    inmap_operating=inmap('operating'), inmap_all=inmap('all'),
    context=gold_h['context'],
    esmi=dict(districts=len(esmi_rows), districts_urban=sum(1 for r in esmi_rows if int(r['n_urban_monitors']) > 0),
              monitors=sum(1 for r in esmi_mon if r['used'] == 'True'), first=min(r['first_date'] for r in esmi_rows),
              last=max(r['last_date'] for r in esmi_rows),
              hosting_districts=sum(1 for r in esmi_rows if r['hosting_operating'] == 'True'),
              hosting_median_h=round(sorted_median([num(r['interruption_h_per_yr_urban']) for r in esmi_rows if r['hosting_operating'] == 'True'])),   # urban monitors, like-for-like
              nonhosting_urban_median_h=round(sorted_median([num(r['interruption_h_per_yr_urban']) for r in esmi_rows
                                                             if r['hosting_operating'] != 'True' and num(r['interruption_h_per_yr_urban']) is not None]))),
    emission_districts=sum(1 for v in em.values() if sum(v) > 0), plants=len(pfeats),
    osm_points=len(json.load(open(os.path.join(GOLD, 'osm_check.geojson'), encoding='utf-8'))['features']))

# ---- checks against the pipeline tables ---------------------------------------------------------
mw_op = sum(p['mw_it'] for p in fp if p['mw_it'] is not None)
check(abs(mw_op - num(nat['total_mw'])) < 0.5, f'operating MW {mw_op:.1f} != tab15_national {nat["total_mw"]}')
check(abs(sum(p['mw_it_all'] for p in fp) - num(nat_all['total_mw'])) < 0.5, 'all-scope MW != tab15_national')
check(abs(sum(p['e_fac_gwh'] for p in fp if p['e_fac_gwh']) / 1000 - num(nat['e_fac'])) < 0.05, 'operating TWh != tab15_national')
check(abs(sum(s1.values()) / 1000 - num(nat['scope1_gl'])) < 0.05, 'district scope-1 water != tab15_national')
check(abs(H['so2_kt_operating'] - num(mt_op['so2_kt'])) < 0.2, 'InMAP-input SO2 != 15b marginal SO2')
check(sum(d['properties']['dc_count'] for d in dfeats) == H['n_operating'], 'district dc_count total != operating facilities')
check(all(d['properties']['dpm25'] is not None for d in dfeats), 'district without an operating PM2.5 increment')

pub_src = os.path.join(OP, 'public', 'india_datacentres_public.csv')
pub = rd(pub_src)
check(len(pub) == len(fp) and {r['facility_id'] for r in pub} == {p['facility_id'] for p in fp}, 'public table facility_ids != facilities')
check(abs(sum(num(r['mw_it_used_in_paper']) or 0 for r in pub) - mw_op) < 0.5, 'public table MW != operating MW')

# ---- write ----------------------------------------------------------------------------------------
os.makedirs(APP, exist_ok=True)
def gj(name, fs):
    with open(os.path.join(APP, name), 'w', encoding='utf-8') as fh:
        json.dump(dict(type='FeatureCollection', features=fs), fh, ensure_ascii=False, separators=(',', ':'))
gj('facilities.geojson', feats); gj('districts.geojson', dfeats); gj('plants.geojson', pfeats)
shutil.copyfile(os.path.join(GOLD, 'osm_check.geojson'), os.path.join(APP, 'osm_check.geojson'))
with open(os.path.join(APP, 'policies.csv'), 'w', newline='', encoding='utf-8') as fh:
    w = csv.DictWriter(fh, fieldnames=pcols); w.writeheader(); w.writerows(pol)
shutil.copyfile(os.path.join(ROOT, 'data', 'state_dc_policies_sources.csv'), os.path.join(APP, 'policy_sources.csv'))
shutil.copyfile(pub_src, os.path.join(APP, 'india_datacentres_public.csv'))
shutil.copyfile(ESMI_TAB, os.path.join(APP, 'district_power_interruptions.csv'))
shutil.copyfile(os.path.join(ROOT, 'data', 'derived', 'esmi_monitor_outages.csv'), os.path.join(APP, 'esmi_monitor_power_interruptions.csv'))
with open(os.path.join(APP, 'headline.json'), 'w', encoding='utf-8') as fh: json.dump(H, fh, indent=2, ensure_ascii=False)

# ---- environmental-justice comparison panel (app tab "Who benefits, who bears") -------------------
# The paper's distributional tables, verbatim from the pipeline: 20b (receiving-side disparity, both scopes),
# 20a (siting and baseline-exposure disparity, S6.2), 20a group exposure (S6.1), tab1 (hosting vs
# non-hosting profile), 16 receiving profile and 20b regional. The app recomputes nothing here.
def rows(dirn, name, keep=None, flt=None):
    f = os.path.join(dirn, name)
    if not os.path.exists(f): return []
    out = []
    for r in rd(f):
        if flt and not flt(r): continue
        def val(k, v):
            if k in ('contrast', 'exposure', 'variable', 'group', 'zone', 'scope', 'inventory', 'star'): return v
            try: x = float(v)
            except (TypeError, ValueError): return v
            return None if x != x else x
        out.append({k: val(k, v) for k, v in r.items() if keep is None or k in keep})
    return out
EJ = dict(
    disparity={sc: rows(d, 'tab20b_inmap_disparity_national.csv', ['contrast', 'pwc_group', 'pwc_ref', 'Z', 'ratio', 'boot_sign_prob'])
               for sc, d in (('operating', OP), ('all', ALL))},
    baseline={sc: rows(d, 'tab20a_disparity_national.csv', ['exposure', 'contrast', 'pwc_group', 'pwc_ref', 'Z', 'ratio', 'boot_sign_prob', 'perm_p_within_state'],
                       flt=lambda r: r['exposure'] in ('Data-center presence', 'PM2.5 (ACAG)', 'Surface NO2'))
              for sc, d in (('operating', OP), ('all', ALL))},   # siting contrasts per scope, so the EJ tab switches every quantity together
    group_exposure=rows(OP, 'tab20a_group_exposure_S61.csv'),
    profile=rows(OP, 'tab1_dc_vs_nodc_profile.csv', ['variable', 'hosting_mean', 'nonhosting_mean', 'd_all_india', 'd_within_state', 'p_within_state']),
    receiving={sc: (rows(d, 'tab16_receiving_profile.csv') or [None])[0] for sc, d in (('operating', OP), ('all', ALL))},
    regional={sc: rows(d, 'tab20b_inmap_regional.csv', ['zone', 'districts', 'population_million', 'pw_increment', 'rural_over_urban'])
              for sc, d in (('operating', OP), ('all', ALL))},
    deaths_total={sc: round(sum((num(r['deaths']) or 0) for r in rd(d, 'tab16_inmap_district.csv')), 2) for sc, d in (('operating', OP), ('all', ALL))},
)
check(all(len(EJ['disparity'][sc]) == 10 and len(EJ['baseline'][sc]) == 30 for sc in ('operating', 'all')), 'EJ panel: expected 10 contrasts and 30 baseline rows per scope')
with open(os.path.join(APP, 'ej_panel.json'), 'w', encoding='utf-8') as fh: json.dump(EJ, fh, indent=1, ensure_ascii=False)

# ---- documented facility dataset (one ZIP for download) -----------------------------------------
# Every URL cited in the public table, with the facilities that cite it and its archived copy.
arch = {r['url']: r for r in rd(ROOT, 'data', 'public', 'source_archive.csv')}
cites = defaultdict(set)
for r in pub:
    for c in ('source_url', 'source_url_2'):
        if r[c].strip(): cites[r[c].strip()].add(r['facility_id'])
def status(m):
    if m in ('saved', 'existing', 'saved_manual'): return 'archived (copy from Sep 2026 or later)'
    if m == 'existing_older': return 'archived (older copy only)'
    if 'blocks' in m or '520' in m: return 'not archived: the site blocks web archives'
    if '523' in m: return 'not archived: the site was unreachable from the archive'
    return 'not archived'
def service(u):
    return 'Internet Archive' if 'web.archive.org' in u else ('archive.today' if re.search(r'archive\.(today|ph|is)', u) else '')
def snapdate(r):
    t = r.get('snapshot') or ''
    m = re.search(r'/(\d{4})\.(\d{2})\.(\d{2})-\d{6}/', r.get('archive_url') or '')
    if m: return '-'.join(m.groups())
    return f'{t[:4]}-{t[4:6]}-{t[6:8]}' if len(t) >= 8 else ''
src_rows = []
for u in sorted(cites):
    a = arch.get(u, {})
    au = a.get('archive_url', '') if not (a.get('method') or '').startswith('failed') else ''
    src_rows.append(dict(url=u, cited_by_facility_ids=' '.join(sorted(cites[u])), archive_url=au, archive_service=service(au),
                         archive_snapshot_date=snapdate(a) if au else '', archive_status=status(a.get('method', ''))))
n_arch = sum(1 for r in src_rows if r['archive_url'])
n_fac_none = sum(1 for r in pub if not r['source_url_archive'].strip() and not r['source_url_2_archive'].strip())
n_dcm = sum(1 for r in src_rows if 'datacentermap.com' in r['url'])
n_dcm_at = sum(1 for r in src_rows if 'datacentermap.com' in r['url'] and r['archive_service'] == 'archive.today')
n_im_at = sum(1 for r in src_rows if 'indiamart' in r['url'] and r['archive_service'] == 'archive.today')
n_miss = len(src_rows) - n_arch

readme = f"""India data-centre facility inventory (revised 30 September 2026)
================================================================
Companion data to: deSouza, P., et al. "The Environmental Geography of Data Centers in India."
Explorer and code:
https://github.com/pdez90/india-datacentre-ej   Contact: priyanka.desouza@ucdenver.edu
Packaged {date.today().isoformat()}.

Licence: CC BY-NC 4.0 (https://creativecommons.org/licenses/by-nc/4.0/). Credit required;
no commercial use without permission.

Please cite as:
  deSouza, P., et al. (2026). India data-centre inventory (public facility table), version
  2026-09-30 [Data set]. https://github.com/pdez90/india-datacentre-ej

Files
-----
india_datacentres_public.csv   {len(pub)} facilities, one row each: name, operator, type, status, city,
                               district and state, reported and modelled IT capacity, the source
                               URL(s) with archived copies, how each row was verified, confidence,
                               corrections and open issues, and the cross-check against the Atlas
                               of Data Center Politics. Site coordinates with their precision class.
sources_and_archives.csv       every URL cited ({len(src_rows)}), the facilities that cite it, and its archived
                               copy (Internet Archive or archive.today) with the snapshot date.
DATA.md                        data dictionary and methods: what a row is, where rows come from,
                               every column, how capacity was set, and the corrections made.
power_interruptions_backup_diesel.csv (+ _README.txt)
                               per hosting district: ESMI interruption hours, the hours the
                               backup-diesel model used and why, and the resulting emissions.
emission_factors.csv (+ _README.txt)
                               every emission and water factor used, with unit, source and script.

Archiving
---------
{n_arch} of {len(src_rows)} cited URLs have an archived copy. {len(pub) - n_fac_none} of {len(pub)} facilities have at least one
archived source. Data Center Map pages the Internet Archive could not capture ({n_dcm_at} of {n_dcm}) and {n_im_at} IndiaMART
pages were archived on archive.today by hand by 10 October 2026. {'The URL' if n_miss == 1 else f'The {n_miss} URLs'} without a copy {'is' if n_miss == 1 else 'are'} on Indian government or
company servers reachable only from Indian networks, so neither archive could capture {'it' if n_miss == 1 else 'them'}; {'it is' if n_miss == 1 else 'each is'} a facility's
second source, and source_note in the facility table records what the authors hold for it. Links were checked and
archived between 30 September and 20 October 2026.
"""
bundle = os.path.join(APP, 'india_datacentres_documented.zip')
with zipfile.ZipFile(bundle, 'w', zipfile.ZIP_DEFLATED) as z:
    z.writestr('README.txt', readme)
    z.write(pub_src, 'india_datacentres_public.csv')
    z.write(os.path.join(ROOT, 'india-datacentre-ej', 'DATA.md'), 'DATA.md')
    buf = io.StringIO(); w = csv.DictWriter(buf, fieldnames=list(src_rows[0].keys())); w.writeheader(); w.writerows(src_rows)
    z.writestr('sources_and_archives.csv', buf.getvalue())
    # the two documented download tables from scripts/30 (power interruptions -> backup diesel; emission factors); 30 runs before 22 in run_all
    for fn in ('power_interruptions_backup_diesel.csv', 'power_interruptions_backup_diesel_README.txt', 'emission_factors.csv', 'emission_factors_README.txt'):
        f30 = os.path.join(ROOT, 'outputs', 'public', fn)
        if not os.path.exists(f30): fail(f'22: {f30} missing; run scripts/30_app_download_tables.R before 22')
        z.write(f30, fn)

print(f'22: wrote {APP}')
print(f'  documented dataset: {os.path.basename(bundle)} ({os.path.getsize(bundle) // 1024} kB); {n_arch}/{len(src_rows)} cited URLs archived; '
      f'{n_fac_none} facilities with no archived source')
print(f'  facilities {H["n_all"]} ({H["n_operating"]} operating, {H["n_construction"]} construction, {H["n_announced"]} announced); '
      f'hosting districts {H["hosting_districts_operating"]} / {H["hosting_districts_all"]}')
print(f'  MW {H["mw_operating"]} / {H["mw_all"]}; {H["twh_operating"]} TWh; CO2 {H["co2_mt_avg_operating"]} avg / {H["co2_mt_marginal_operating"]} marginal Mt; '
      f'SO2/NOx/PM2.5 {H["so2_kt_operating"]}/{H["nox_kt_operating"]}/{H["pm25_kt_operating"]} kt')
print(f'  power interruptions: {H["esmi"]["districts"]} ESMI districts ({H["esmi"]["monitors"]} monitors); hosting median {H["esmi"]["hosting_median_h"]} h/yr '
      f'({H["esmi"]["hosting_districts"]} districts) vs non-hosting urban {H["esmi"]["nonhosting_urban_median_h"]}')
for sc in ('operating', 'all'):
    m = H['inmap_' + sc]
    print(f'  InMAP {sc}: {m["pwm_ugm3"]} ug/m3 district-mean (grid {m["pwm_grid_ugm3"]}), {m["pop_ge_0_01_million"]} M >= 0.01, {m["deaths_per_yr"]} deaths')
print(f'  plants {H["plants"]} in {H["emission_districts"]} districts; policy states {H["policy_states_dedicated"]}/{H["policy_states_reviewed"]}, '
      f'facilities in policy states {H["facilities_in_policy_states"]} / {H["facilities_in_policy_states_all"]}')
