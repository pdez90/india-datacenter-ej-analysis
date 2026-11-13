"""19c_figure_s83.py - Figure S8.3: the chain from facility to receiving communities (schematic).
The one number drawn (population of the coal-hosting districts) is read from the analysis frame. Added 18 Oct 2026;
replaces a hand-drawn schematic that carried a stale 397 M. Writes paper/figs/ms_s83_chain.{png,pdf,svg}."""
import os, csv
try:
    import matplotlib
except ImportError:                                  # the figure is optional for a rerun: keep the committed one
    print(f'{__file__}: matplotlib not installed (pip install matplotlib); figure not redrawn'); raise SystemExit(0)
matplotlib.use('Agg')
import matplotlib.pyplot as plt
from matplotlib.patches import FancyBboxPatch
from matplotlib import font_manager as fm

ROOT = os.environ.get('DCEJ_ROOT') or os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
fr = list(csv.DictReader(open(os.path.join(ROOT, 'data', 'derived', 'analysis_district.csv'), encoding='utf-8')))
coal = [r for r in fr if float(r['coal_mw'] or 0) > 0]
N_COAL, POP_COAL = len(coal), sum(float(r['pop_million']) for r in coal)
assert N_COAL == 133 and round(POP_COAL) == 414, (N_COAL, POP_COAL)      # section S4: 133 districts, 414 million people (Solapur GPPD coordinate corrected 30 Oct 2026)

for f in ('/usr/share/fonts/truetype/liberation/LiberationSans-Regular.ttf', '/usr/share/fonts/truetype/liberation/LiberationSans-Bold.ttf'):
    if os.path.exists(f): fm.fontManager.addfont(f)
plt.rcParams.update({'font.family': 'sans-serif', 'font.sans-serif': ['Arial', 'Helvetica', 'Liberation Sans', 'DejaVu Sans'],
                     'font.size': 7.2, 'pdf.fonttype': 42, 'svg.fonttype': 'none'})
INK, MID = '#222222', '#6b6b6b'
W, H = 100, 62
fig = plt.figure(figsize=(6.5, 6.5 * H / W)); ax = fig.add_axes([0, 0, 1, 1]); ax.set_xlim(0, W); ax.set_ylim(0, H); ax.axis('off')
STEPS = [  # title, detail, fill, text colour, observed?
    ('Data-center facility', 'Affluent urban district; continuous IT load, cooling, diesel backup', '#1f3b5c', 'white', True),
    ('Local burdens', 'Co-located urban air pollution, land, construction, induced activity', '#dfe6ee', INK, True),
    ('Shared regional systems', 'Airshed; river basin and aquifer; electricity grid', '#f3f3f3', INK, False),
    ('Generation corridor', 'Coal-dominated marginal generation: combustion emissions, cooling water', '#f3f3f3', INK, False),
    ('Receiving communities', f'Poorer, more rural, more SC/ST districts; {POP_COAL:,.0f} million people in the {N_COAL} coal-hosting districts',
     '#fbe3d6', INK, False),
]
x0, x1, h, gap, top = 2, 76, 9.0, 3.0, 59.5
ys = []
for i, (t, d, fc, tc, obs) in enumerate(STEPS):
    y1 = top - i * (h + gap); y0 = y1 - h; ys.append((y0, y1))
    ax.add_patch(FancyBboxPatch((x0, y0), x1 - x0, h, boxstyle='round,pad=0,rounding_size=0.8', fc=fc, ec='#1f3b5c' if i < 2 else MID,
                                lw=0.6, ls='-' if obs else (0, (3, 2))))
    ax.text(x0 + 2.2, y1 - 3.0, t, fontweight='bold', color=tc, va='center')
    ax.text(x0 + 2.2, y1 - 6.2, d, color=tc, va='center')
    if i:
        ax.annotate('', xy=((x0 + x1) / 2, y1), xytext=((x0 + x1) / 2, ys[i - 1][0]),
                    arrowprops=dict(arrowstyle='-|>,head_length=0.5,head_width=0.22', color=INK, lw=0.7, shrinkA=0, shrinkB=0))
def bracket(ya, yb, lab, ls):
    ax.plot([78, 79.5, 79.5, 78], [yb, yb, ya, ya], color=MID, lw=0.7, ls=ls)
    ax.text(81, (ya + yb) / 2, lab, va='center', ha='left', color=MID, style='italic')
bracket(ys[1][0], ys[0][1], 'Observed\ndirectly', '-')
bracket(ys[4][0], ys[2][1], 'Inferred from grid\ncomposition,\nmarginal factors\nand scenario\naccounting', (0, (3, 2)))
out = os.path.join(ROOT, 'paper', 'figs'); os.makedirs(out, exist_ok=True)
for ext, kw in (('png', {'dpi': 600}), ('pdf', {}), ('svg', {})):
    fig.savefig(os.path.join(out, f'ms_s83_chain.{ext}'), facecolor='white', **kw)
print('19c: wrote ms_s83_chain.{png,pdf,svg}:', N_COAL, 'coal-hosting districts,', round(POP_COAL), 'million')
