"""19b_figure1.py - Figure 1: how the facility inventory (A) and its IT capacity (B) were built.
Every number is read from the pipeline's tables (current inventory, operating scope) and checked; drawn with matplotlib
and written to paper/figs/ms_fig1_inventory_flow.{png,pdf,svg} (PNG at 600 dpi). Added 18 Oct 2026 to replace the
ggplot version formerly drawn by 19_figures.R. Run: python3 scripts/19b_figure1.py  (DCEJ_ROOT overrides the project root)."""
import os, csv, collections
try:
    import matplotlib
except ImportError:                                  # the figure is optional for a rerun: keep the committed one
    print(f'{__file__}: matplotlib not installed (pip install matplotlib); figure not redrawn'); raise SystemExit(0)
matplotlib.use('Agg')
import matplotlib.pyplot as plt
from matplotlib.patches import FancyBboxPatch, Rectangle, Circle
from matplotlib import font_manager as fm

ROOT = os.environ.get('DCEJ_ROOT') or os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
O = lambda f: os.path.join(ROOT, 'outputs', f)
rd = lambda f: list(csv.DictReader(open(f, encoding='utf-8')))
num = lambda x: float(x) if x not in ('', 'NA', None) else None

pub = rd(O('public/india_datacentres_public.csv'))
cor = rd(os.path.join(ROOT, 'data', 'public', 'inventory_corrections_v2.csv'))
fx = [r for r in rd(O('tab15_facilities.csv')) if r['status'] == 'operating']
nat = {r['scenario']: r for r in rd(O('tab15_national.csv'))}['central']

def src_counts(prefix):
    rows = [r for r in pub if r['inventory_source'].lower().startswith(prefix)]
    return len(rows), sum(r['status'] == 'operating' for r in rows)
S_DCM, S_CEN, S_GOV, S_ATL = (src_counts(p) for p in ('data center map', 'operator census', 'government', 'operator, company'))
N_ALL = len(pub); st = collections.Counter(r['status'] for r in pub)
N_OP, N_UC, N_AN = st['operating'], sum(v for k, v in st.items() if 'construction' in k), sum(v for k, v in st.items() if 'announced' in k)
N_DROP = sum(r['field'] == '_drop' for r in cor); N_EDIT = len(cor) - N_DROP
N_EDIT_FAC = len({r['facility_id'] for r in cor if r['field'] != '_drop'})
N_ADP = sum(r['adp_match'] != 'none' for r in pub)
N_OSM = 20                                           # screened OpenStreetMap points (section S1.1.3; app check_app_data.R)
rep = [r for r in fx if (num(r['mw_reported']) or 0) > 0]
N_REP, MW_REP = len(rep), sum(num(r['mw_reported']) for r in rep)
# capacity-method tier of each operating row with a published figure (data/public/CAPACITY_METHOD.md; tier named at the
# start of mw_basis): T1 IT load as published; T2-T5 derived by stated rules (unlabelled capacity, total power / PUE,
# company or campus share, racks x 4 kW)
import re as _re
_tier = collections.Counter((_re.match(r'(?:current [\d.]+ MW: )?(T\d)', p['mw_basis']) or [None, '?'])[1]
                            for p in pub if p['status'] == 'operating' and p['mw_it_reported_current'] not in ('', 'NA'))
N_T1 = _tier['T1']; N_DER = sum(v for k, v in _tier.items() if k in ('T2', 'T3', 'T4', 'T5'))
assert N_T1 + N_DER == N_REP, (dict(_tier), N_REP)
COM = ('colocation', 'hyperscale')
com = [r for r in fx if r['dc_type'] in COM]; non = [r for r in fx if r['dc_type'] not in COM]
types = collections.Counter(r['dc_type'] for r in fx)
MW = lambda rows: sum(num(r['mw_it']) for r in rows)
MW_COM, MW_NON, MW_TOT = MW(com), MW(non), MW(fx)
REP_COM = sum(num(r['mw_reported']) for r in com if (num(r['mw_reported']) or 0) > 0)
REP_NON = sum(num(r['mw_reported']) for r in non if (num(r['mw_reported']) or 0) > 0)
ANCHOR = num(nat['anchor_mw']); PIPE = sum(num(r['mw_it_all']) for r in rd(O('tab15_facilities.csv')) if r.get('mw_it_all')) if 'mw_it_all' in fx[0] else None
# stock-plus-pipeline total from the all-scope national table (summing the public table's 3-dp rounded column
# can fall on the other side of a .5 boundary: 6,301.4 vs 6,301.5 after the 19 Oct 2026 IN121 correction)
_nat_all = os.path.join(ROOT, 'outputs_all', 'tab15_national.csv')
if os.path.exists(_nat_all): PIPE = num({r['scenario']: r for r in rd(_nat_all)}['central']['total_mw'])
PIPE = PIPE or sum(num(r['mw_it_used_in_pipeline_scenario']) for r in pub if r.get('mw_it_used_in_pipeline_scenario') not in ('', None, 'NA'))
# 29 Oct 2026: pins updated to the batch-9 inventory (329 / 233; capacity method CAPACITY_METHOD.md)
# checks against the paper's text (Figure 1 caption, sections 2.1 and S1.1, S4)
assert (N_ALL, N_OP, N_UC, N_AN) == (329, 233, 60, 36), (N_ALL, N_OP, N_UC, N_AN)
assert (S_DCM, S_CEN, S_GOV, S_ATL) == ((202, 123), (66, 62), (32, 32), (29, 16)), (S_DCM, S_CEN, S_GOV, S_ATL)
assert (N_DROP, N_EDIT, N_EDIT_FAC, N_ADP) == (29, 310, 170, 243), (N_DROP, N_EDIT, N_EDIT_FAC, N_ADP)
assert N_REP == 123 and round(MW_REP) == 1383 and abs(MW_COM - ANCHOR) < 0.5 and round(MW_NON) == 251 and round(MW_TOT) == 2051, (N_REP, MW_REP, MW_COM, MW_NON, MW_TOT)
assert (len(com), types['colocation'], types['hyperscale'], len(non)) == (134, 127, 7, 99)
assert round(PIPE) == 8114, PIPE
assert (N_T1, N_DER) == (93, 30), (N_T1, N_DER)
c = lambda x: f'{x:,.0f}'

for f in ('/usr/share/fonts/truetype/liberation/LiberationSans-Regular.ttf', '/usr/share/fonts/truetype/liberation/LiberationSans-Bold.ttf'):
    if os.path.exists(f): fm.fontManager.addfont(f)
plt.rcParams.update({'font.family': 'sans-serif', 'font.sans-serif': ['Arial', 'Helvetica', 'Liberation Sans', 'DejaVu Sans'],
                     'font.size': 6.6, 'pdf.fonttype': 42, 'svg.fonttype': 'none'})
INK, MID, LIGHT = '#222222', '#6b6b6b', '#bdbdbd'
NAVY, NAVY_L, PALE = '#1f3b5c', '#dfe6ee', '#f3f3f3'
W, H, Y0 = 100, 52.5, 5.5
fig = plt.figure(figsize=(7.2, 7.2 * H / W))
ax = fig.add_axes([0, 0, 1, 1]); ax.set_xlim(0, W); ax.set_ylim(Y0, Y0 + H); ax.axis('off')

def box(x0, y0, x1, y1, lines, fc='white', ec=INK, tc=INK, lw=0.6, ls='-', align='center', pad=1.0):
    ax.add_patch(FancyBboxPatch((x0, y0), x1 - x0, y1 - y0, boxstyle='round,pad=0,rounding_size=0.6',
                                fc=fc, ec=ec, lw=lw, ls=ls, zorder=2))
    n = len(lines); step = 2.15; ytop = (y0 + y1) / 2 + (n - 1) * step / 2
    x = (x0 + x1) / 2 if align == 'center' else x0 + pad
    for i, (txt, bold) in enumerate(lines):
        ax.text(x, ytop - i * step, txt, ha=align, va='center', color=tc, zorder=3,
                fontweight='bold' if bold else 'normal')

def arrow(x0, y0, x1, y1, ls='-', color=INK):
    ax.annotate('', xy=(x1, y1), xytext=(x0, y0), zorder=1,
                arrowprops=dict(arrowstyle='-|>,head_length=0.5,head_width=0.22', color=color, lw=0.7,
                                linestyle=ls, shrinkA=0, shrinkB=0))

def line(xs, ys, color=INK, lw=0.7, ls='-'):
    ax.plot(xs, ys, color=color, lw=lw, ls=ls, solid_capstyle='butt', zorder=1)

def header(x, y, letter, title):
    ax.text(x, y, letter, fontsize=9, fontweight='bold', va='baseline', color=INK)
    ax.text(x + 2.6, y, title, fontsize=7.6, fontweight='bold', va='baseline', color=INK)

# ---------------- Panel A ----------------
header(0.5, 55.2, 'A', 'Building the facility inventory')
ax.text(12.5, 51.3, 'Primary sources (rows entered)', ha='center', color=MID, fontsize=6.2, style='italic')
SRC = [  # (center y, height, lines)
    (46.0, 6.6, [('DataCenterMap directory', True), (f'{S_DCM[0]} rows ({S_DCM[1]} operating)', False)]),
    (37.6, 6.6, [('Operator census (15 operators)', True), (f'{S_CEN[0]} rows ({S_CEN[1]} operating)', False)]),
    (29.2, 6.6, [('Government data-center census', True), (f'{S_GOV[0]} rows ({S_GOV[1]} operating)', False)]),
    (19.6, 8.8, [('Sites found via the Atlas cross-check,', True), ('entered on primary records', True),
                 (f'{S_ATL[0]} rows ({S_ATL[1]} operating)', False)]),
]
SX0, SX1, BUS = 0.5, 24.5, 26.6
for cy, h, lines in SRC:
    box(SX0, cy - h / 2, SX1, cy + h / 2, lines, fc=PALE, ec=LIGHT)
    line([SX1, BUS], [cy, cy])
line([BUS, BUS], [SRC[-1][0], SRC[0][0]])
INV = (29.5, 26.5, 49.5, 39.5)          # x0, y0, x1, y1
inv_cy = (INV[1] + INV[3]) / 2
arrow(BUS, inv_cy, INV[0], inv_cy)
box(*INV, [('Facility inventory', True), (f'{N_ALL} facilities', True), (f'{N_OP} operating', False),
           (f'{N_UC} under construction', False), (f'{N_AN} announced', False)], fc=NAVY, ec=NAVY, tc='white')
# corrections (above) and cross-checks (below)
box(29.5, 44.0, 49.5, 51.6, [('Sourced corrections', True), (f'{N_DROP} removals; {N_EDIT} field edits', False),
                             (f'on {N_EDIT_FAC} facilities', False)], ec=INK)
arrow(39.5, 44.0, 39.5, INV[3])
box(29.5, 7.6, 49.5, 18.4, [('Cross-checks (add no rows)', True), ('Atlas of Data Center Politics:', False),
                            (f'{N_ADP} of {N_ALL} rows matched', False), (f'OpenStreetMap screen: {N_OSM} points', False)],
    ec=MID, ls=(0, (3, 2)))
arrow(39.5, 18.4, 39.5, INV[1], ls=(0, (3, 2)), color=MID)
ax.text(40.3, 22.4, 'corroborate', ha='left', va='center', color=MID, fontsize=6.0, style='italic')

# ---------------- Panel B ----------------
BX = 56.5
header(BX - 1.5, 55.2, 'B', 'Sizing IT capacity of the operating stock')
SPINE = 55.0
STEPS = [
    (48.8, 8.8, [('Published capacity used directly', True), (f'{N_REP} facilities, {c(MW_REP)} MW: {N_T1} IT load as published,', False),
                 (f'{N_DER} derived by stated rules (method tiers T2-T5)', False)]),
    (39.1, 8.8, [(f'Commercial stock scaled to {c(ANCHOR)} MW anchor', True),
                 (f"{len(com)} facilities ({types['colocation']} colocation, {types['hyperscale']} hyperscale);", False),
                 (f'residual {c(MW_COM - REP_COM)} MW by weight, hyperscale 10 : colocation 3', False)]),
    (28.2, 8.8, [('Non-commercial sized at the same rate', True),
                 (f'{len(non)} facilities; weights enterprise 1, government 1,', False),
                 (f'telecom 0.5: {c(MW_NON)} MW, not rescaled', False)]),
]
for i, (cy, h, lines) in enumerate(STEPS):
    box(BX + 1.5, cy - h / 2, 99.5, cy + h / 2, lines, fc=NAVY_L, ec=NAVY, align='left', pad=4.0)
    ax.add_patch(Circle((BX + 3.6, cy + (len(lines) - 1) * 2.15 / 2), 1.05, fc=NAVY, ec='none', zorder=4))
    ax.text(BX + 3.6, cy + (len(lines) - 1) * 2.15 / 2, str(i + 1), ha='center', va='center', color='white',
            fontsize=5.8, fontweight='bold', zorder=5)
    arrow(SPINE, cy, BX + 1.5, cy)
line([SPINE, SPINE], [STEPS[-1][0], STEPS[0][0]])
line([INV[2], SPINE], [inv_cy + 2.5, inv_cy + 2.5])     # operating stock feeds panel B
ax.text((INV[2] + SPINE) / 2, inv_cy + 3.4, str(N_OP), ha='center', va='bottom', color=MID, fontsize=6.2)

# composition bar of the 2,059 MW operating stock
segs = [(REP_COM, 'rep'), (MW_COM - REP_COM, 'mod'), (REP_NON, 'rep'), (MW_NON - REP_NON, 'mod')]
total = sum(s for s, _ in segs); x0b, x1b = BX + 1.5, 99.5; yb, hb = 13.4, 3.2
sc = (x1b - x0b) / total; x = x0b; edges = [x]
for v, kind in segs:
    ax.add_patch(Rectangle((x, yb), v * sc, hb, fc=NAVY if kind == 'rep' else '#8fa6bf', ec='white', lw=0.5, zorder=2))
    x += v * sc; edges.append(x)
ax.text(x0b, 21.1, f'Operating stock: {c(MW_TOT)} MW', fontweight='bold', va='center')
def bracket(xa, xb, label, ha='center'):
    yb2 = yb + hb + 0.7
    line([xa, xa, xb, xb], [yb2, yb2 + 0.9, yb2 + 0.9, yb2], color=MID, lw=0.6)
    ax.text(xa if ha == 'left' else xb, yb2 + 1.7, label, ha=ha, va='center', color=INK, fontsize=6.2)
bracket(edges[0], edges[2] - 0.3, f'Commercial {c(MW_COM)} MW (anchored)', ha='left')
bracket(edges[2] + 0.3, edges[4], f'Non-commercial {c(MW_NON)} MW', ha='right')
# legend
for j, (fc, lab) in enumerate([(NAVY, f'Published {c(MW_REP)} MW'), ('#8fa6bf', f'Modeled {c(MW_TOT - MW_REP)} MW')]):
    lx = x0b + j * 16.0
    ax.add_patch(Rectangle((lx, 9.4), 1.8, 1.4, fc=fc, ec='none'))
    ax.text(lx + 2.6, 10.1, lab, va='center', fontsize=6.2)
ax.text(x0b, 7.0, f'Stock plus pipeline ({N_ALL} facilities): {c(PIPE)} MW, a scenario, not anchored',
        va='center', color=MID, fontsize=6.0, style='italic')

out = os.path.join(ROOT, 'paper', 'figs'); os.makedirs(out, exist_ok=True)
for ext, kw in (('png', {'dpi': 600}), ('pdf', {}), ('svg', {})):
    fig.savefig(os.path.join(out, f'ms_fig1_inventory_flow.{ext}'), facecolor='white', **kw)
print('19b: wrote ms_fig1_inventory_flow.{png,pdf,svg}:', N_ALL, 'facilities,', N_OP, 'operating,', c(MW_TOT), 'MW')
