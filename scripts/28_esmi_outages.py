#!/usr/bin/env python3
"""28_esmi_outages.py -- district power-supply interruption hours from the Prayas ESMI record.

Source: Prayas (Energy Group), Electricity Supply Monitoring Initiative (ESMI), minute-wise voltage
data 2014-2019, Harvard Dataverse doi:10.7910/DVN/CLLZZM. Unzip the 14 files into data/esmi/.
Terms: free for non-commercial, academic and research use with the source acknowledged.

Method (SI S5; DCGRD-India dcgrd_02 rebuilt 2 Oct 2026 after the 29 Sep loss)
  - A minute with recorded voltage below 80 V is an interruption. This catches loss of supply (0 V)
    and severe undervoltage. A blank minute is missing data, NOT an interruption (Prayas guidance).
  - Rows with Hour outside 0-23 or an unreadable date are dropped; a (monitor, date, hour) repeated
    across files is counted once.
  - Per monitor: interruption hours per year = interrupted minutes / recorded minutes x 8,760.
    This annualises over the minutes actually recorded, so monitors with gaps are comparable.
  - Monitors with fewer than MIN_DAYS recorded days are listed but not used in district values.
  - District = mean over its monitors (each monitor weighted equally), for all monitors and
    separately for urban monitors (State Capital, District Headquarters, Other Municipal Area)
    and rural monitors (Gram Panchayat).
  - ESMI districts are mapped to the 2015 NFHS district geography the paper uses. Newer districts
    are pooled into their 2015 parent (Hapur -> Ghaziabad, Palghar -> Thane, Salcete -> South Goa),
    and, for the v1 inventory only, the metro conventions are applied (Chennai -> Kancheepuram, Hyderabad ->
    Rangareddy, Kolkata -> South Twenty Four Parganas); under v2 these cities are their own Census 2011 districts.
  - Only monitors listed in ESMI's location file are used (district and category come from it);
    voltage records for unlisted names are reported and excluded.

Writes
  data/derived/esmi_monitor_outages.csv     one row per monitor
  outputs/tab28_esmi_district_outages.csv   one row per ESMI district (2015 geography), with hosting flags
  outputs/tab28_esmi_hourofday.csv          share of recorded minutes interrupted, by hour of day
Needs pandas. Run from the project root:  python3 scripts/28_esmi_outages.py
"""
import glob, os, re, sys
import numpy as np
import pandas as pd

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
ESMI = os.path.join(ROOT, 'data', 'esmi')
YEARS = None          # None = the full record (2014-2019), as in SI Table S5.1; e.g. [2018, 2019] for a recent-years variant
MIN_DAYS = 30         # monitors with fewer recorded days are excluded from district values
THRESH_V = 80
URBAN = {'State Capital', 'District Headquarters', 'Other Municipal Area'}

def fail(m): sys.exit('28: CHECK FAILED - ' + m)
norm_loc = lambda s: re.sub(r'[^a-z0-9]', '', re.sub(r'\[offline\]', '', str(s).lower()))
norm = lambda s: re.sub(r'[^a-z]', '', str(s).lower())

# ESMI district -> 2015 NFHS district (name, state). Exact name matches need no entry.
XWALK = {
    ('Bengaluru Urban', 'Karnataka'): ('Bangalore', 'Karnataka'),
    ('Kamrup Metro', 'Assam'): ('Kamrup Metropolitan', 'Assam'),
    ('Sasaram', 'Bihar'): ('Rohtas', 'Bihar'),
    ('Kanpur', 'Uttar Pradesh'): ('Kanpur Nagar', 'Uttar Pradesh'),
    ('Salcete', 'Goa'): ('South Goa', 'Goa'),
    ('Palghar', 'Maharashtra'): ('Thane', 'Maharashtra'),
    ('Hapur', 'Uttar Pradesh'): ('Ghaziabad', 'Uttar Pradesh'),
    ('Anantpur', 'Andhra Pradesh'): ('Anantapur', 'Andhra Pradesh'),
    ('Gautam Buddh Nagar', 'Uttar Pradesh'): ('Gautam Buddha Nagar', 'Uttar Pradesh'),
    ('North 24 Parganas', 'West Bengal'): ('North Twenty Four Parganas', 'West Bengal'),
    ('Bijapur', 'Karnataka'): ('Bijapur', 'Karnataka'),
    ('Mumbai Suburban District', 'Maharashtra'): ('Mumbai Suburban', 'Maharashtra'),
    ('North East Delhi', 'Delhi'): ('North East', 'Nct Of Delhi'),
    ('South Delhi', 'Delhi'): ('South', 'Nct Of Delhi'),
    ('Central Delhi', 'Delhi'): ('Central', 'Nct Of Delhi'),
    ('East Delhi', 'Delhi'): ('East', 'Nct Of Delhi'),
    ('North West Delhi', 'Delhi'): ('North West', 'Nct Of Delhi'),
    ('West Delhi', 'Delhi'): ('West', 'Nct Of Delhi'),
    ('South West Delhi', 'Delhi'): ('South West', 'Nct Of Delhi'),
    ('Hyderabad', 'Telangana'): ('Hyderabad', 'Andhra Pradesh'),   # 2015 geography predates Telangana
}
# v1 (submitted paper) convention: the warped NFHS polygons for Chennai, Hyderabad and Kolkata were
# offshore/empty and carried no facilities, so their monitors were pooled with the neighbouring district.
# v2 uses the Census 2011 boundaries (00a_districts.R): these cities are real districts with facilities.
if os.environ.get('DCEJ_INVENTORY', 'v2') == 'v1':
    XWALK.update({('Chennai', 'Tamil Nadu'): ('Kancheepuram', 'Tamil Nadu'),
                  ('Hyderabad', 'Telangana'): ('Rangareddy', 'Andhra Pradesh'),
                  ('Hyderabad', 'Andhra Pradesh'): ('Rangareddy', 'Andhra Pradesh'),
                  ('Kolkata', 'West Bengal'): ('South Twenty Four Parganas', 'West Bengal')})

# ---- location file ---------------------------------------------------------------------------
lf = glob.glob(os.path.join(ESMI, '*location information*.csv'))
if len(lf) != 1: fail(f'expected one "ESMI location information.csv" in {ESMI}, found {len(lf)}')
loc = pd.read_csv(lf[0]); loc.columns = [c.strip() for c in loc.columns]
for c in ('Location name', 'District', 'State', 'Category', 'Connection Type'): loc[c] = loc[c].astype(str).str.strip()
loc['k'] = loc['Location name'].map(norm_loc)
if loc.k.duplicated().any(): fail('duplicate monitor names in the location file')

ad = pd.read_csv(os.path.join(ROOT, 'data', 'derived', 'analysis_district.csv'))
ad['key'] = ad.dist_name.map(norm) + '|' + ad.state_name_2015.map(norm)
by_name = ad.groupby(ad.dist_name.map(norm))
def to_2015(d, s):
    if (d, s) in XWALK: d, s = XWALK[(d, s)]
    else:
        m = ad[ad.dist_name.map(norm) == norm(d)]
        if len(m) == 1: d, s = m.iloc[0].dist_name, m.iloc[0].state_name_2015
        else: return None
    m = ad[(ad.dist_name.map(norm) == norm(d)) & (ad.state_name_2015.map(norm) == norm(s))]
    return None if len(m) != 1 else m.iloc[0]
rows = []
for (d, s), _ in loc.groupby(['District', 'State']):
    r = to_2015(d, s)
    if r is None: fail(f'ESMI district {d}, {s} has no 2015 match: add it to XWALK')
    rows.append(dict(District=d, State=s, zone_uid=r.zone_uid, dist_name=r.dist_name, state_name=r.state_name_2015))
loc = loc.merge(pd.DataFrame(rows), on=['District', 'State'])

# ---- voltage files -> one row per monitor-hour ---------------------------------------------------
vf = sorted(f for f in glob.glob(os.path.join(ESMI, '*.csv')) if 'voltage' in os.path.basename(f).lower())
if not vf: fail(f'no ESMI voltage files in {ESMI}')
MINS = [f'Min {i}' for i in range(60)]
parts = []
for f in vf:
    for ch in pd.read_csv(f, chunksize=400_000, dtype=str, low_memory=False):
        ch.columns = [c.strip() for c in ch.columns]
        hour = pd.to_numeric(ch['Hour'], errors='coerce')
        d = ch['Date'].astype(str).str.strip()
        dt = pd.to_datetime(d, format='%d-%m-%Y', errors='coerce')
        miss = dt.isna(); dt[miss] = pd.to_datetime(d[miss], format='%m/%d/%Y', errors='coerce')
        v = ch[[c for c in MINS if c in ch.columns]].apply(pd.to_numeric, errors='coerce').to_numpy(dtype='float32')
        p = pd.DataFrame({'k': ch['Location name'].map(norm_loc), 'loc_raw': ch['Location name'].str.strip(), 'date': dt, 'hour': hour,
                          'valid': np.isfinite(v).sum(1).astype('int16'), 'out': (v < THRESH_V).sum(1).astype('int16')})
        parts.append(p[p.hour.between(0, 23) & p.date.notna() & (p.valid > 0)])
    print(f'28: read {os.path.basename(f)}', flush=True)
a = pd.concat(parts, ignore_index=True)
n0 = len(a); a = a.drop_duplicates(['k', 'date', 'hour'])
if YEARS: a = a[a.date.dt.year.isin(YEARS)]
unlisted = a[~a.k.isin(loc.k)].groupby('loc_raw').size()
a = a[a.k.isin(loc.k)]
yr_span = f'{a.date.min():%b %Y}-{a.date.max():%b %Y}'
print(f'28: {len(a):,} monitor-hours ({yr_span}); {n0 - len(a) - unlisted.sum():,} duplicate/out-of-period rows dropped; '
      f'{len(unlisted)} unlisted monitor names excluded ({unlisted.sum():,} hours)')

# ---- per monitor ----------------------------------------------------------------------------------
m = a.groupby('k').agg(recorded_minutes=('valid', 'sum'), interrupted_minutes=('out', 'sum'), recorded_days=('date', 'nunique'),
                       first_date=('date', 'min'), last_date=('date', 'max')).reset_index()
m = loc.merge(m, on='k', how='inner')
m['interruption_h_per_yr'] = m.interrupted_minutes / m.recorded_minutes * 8760
m['urban'] = m.Category.isin(URBAN)
m['used'] = m.recorded_days >= MIN_DAYS
mon = m[['Location name', 'District', 'State', 'Category', 'Connection Type', 'urban', 'zone_uid', 'dist_name', 'state_name',
         'first_date', 'last_date', 'recorded_days', 'recorded_minutes', 'interrupted_minutes', 'interruption_h_per_yr', 'used']].rename(
    columns={'Location name': 'monitor', 'District': 'esmi_district', 'State': 'esmi_state', 'Category': 'esmi_category',
             'Connection Type': 'connection_type', 'dist_name': 'district_2015', 'state_name': 'state_2015'})
mon['first_date'] = mon.first_date.dt.date; mon['last_date'] = mon.last_date.dt.date
mon = mon.sort_values(['state_2015', 'district_2015', 'monitor'])

# ---- per district -----------------------------------------------------------------------------------
u = m[m.used]
def agg(g):
    ur, ru = g[g.urban], g[~g.urban]
    return pd.Series(dict(
        n_monitors=len(g), n_urban_monitors=len(ur), n_rural_monitors=len(ru),
        interruption_h_per_yr=g.interruption_h_per_yr.mean(),
        interruption_h_per_yr_urban=ur.interruption_h_per_yr.mean() if len(ur) else np.nan,
        interruption_h_per_yr_rural=ru.interruption_h_per_yr.mean() if len(ru) else np.nan,
        min_monitor_h=g.interruption_h_per_yr.min(), max_monitor_h=g.interruption_h_per_yr.max(),
        recorded_monitor_days=int(g.recorded_days.sum()),
        first_date=g.first_date.min().date(), last_date=g.last_date.max().date(),
        esmi_districts_pooled='; '.join(sorted(set(g.District)))))
dist = u.groupby(['zone_uid', 'dist_name', 'state_name']).apply(agg, include_groups=False).reset_index()
op = pd.read_csv(os.path.join(ROOT, 'outputs', 'tab15_facilities.csv'))
al = pd.read_csv(os.path.join(ROOT, 'outputs_all', 'tab15_facilities.csv'))
dist['dc_count_operating'] = dist.zone_uid.map(op.zone_uid.value_counts()).fillna(0).astype(int)
dist['dc_count_all'] = dist.zone_uid.map(al.zone_uid.value_counts()).fillna(0).astype(int)
dist['hosting_operating'] = dist.dc_count_operating > 0
dist = dist.rename(columns={'dist_name': 'district_2015', 'state_name': 'state_2015'}).sort_values('interruption_h_per_yr', ascending=False)

# ---- hour of day ------------------------------------------------------------------------------------
au = a[a.k.isin(u.k)].merge(m[['k', 'urban']], on='k')
hod = au.groupby(['hour', 'urban']).agg(rec=('valid', 'sum'), out=('out', 'sum')).reset_index()
hod['share_interrupted'] = hod.out / hod.rec
hod = hod.pivot(index='hour', columns='urban', values='share_interrupted').rename(columns={True: 'urban', False: 'rural'}).reset_index()

# ---- checks and summary -------------------------------------------------------------------------------
if dist.zone_uid.duplicated().any(): fail('a 2015 district appears twice')
if not set(dist.zone_uid) <= set(ad.zone_uid): fail('district outside the 2015 layer')
if (mon.interruption_h_per_yr > 8760).any() or (mon.interruption_h_per_yr < 0).any(): fail('impossible monitor value')
os.makedirs(os.path.join(ROOT, 'data', 'derived'), exist_ok=True)
mon.to_csv(os.path.join(ROOT, 'data', 'derived', 'esmi_monitor_outages.csv'), index=False, float_format='%.1f')
dist.to_csv(os.path.join(ROOT, 'outputs', 'tab28_esmi_district_outages.csv'), index=False, float_format='%.1f')
hod.to_csv(os.path.join(ROOT, 'outputs', 'tab28_esmi_hourofday.csv'), index=False, float_format='%.5f')

h = dist[dist.hosting_operating]; nh = dist[~dist.hosting_operating & (dist.n_urban_monitors > 0)]
q = lambda s: f'median {s.median():.0f} (IQR {s.quantile(.25):.0f}-{s.quantile(.75):.0f}; range {s.min():.0f}-{s.max():.0f})'
print(f'28: {len(m)} monitors with data, {int(m.used.sum())} used (>= {MIN_DAYS} days); {len(dist)} districts, '
      f'{int((dist.n_urban_monitors > 0).sum())} with an urban monitor')
print(f'   urban monitors {q(u[u.urban].interruption_h_per_yr)} h/yr; rural {q(u[~u.urban].interruption_h_per_yr)} h/yr')
print(f'   hosting districts (operating, v2) with ESMI: {len(h)}, {q(h.interruption_h_per_yr)} h/yr')
print(f'   non-hosting districts with an urban monitor: {len(nh)}, urban {q(nh.interruption_h_per_yr_urban)} h/yr')
for z in ('Ghaziabad', 'Thane', 'Gurgaon'):
    r = dist[dist.district_2015 == z]
    if len(r): print(f'   {z}: {r.interruption_h_per_yr.iloc[0]:.1f} h/yr from {r.n_monitors.iloc[0]} monitors')
