#!/usr/bin/env python3
"""27_snapshot_sources.py -- dated local snapshots of source pages that refuse web archives.

Some sites (Baxtel, IndiaMART, Iron Mountain, several gov.in pages ...) turn away the Internet Archive's
crawler (HTTP 520), so 25_archive_sources.py cannot preserve them. For every page in
data/public/archive_manual_todo.csv that still has no archive link (or, with --all, for EVERY source URL
cited in data/public/: facility sources, correction and open-issue evidence), this script opens the page in your own
Google Chrome (headless, from your own internet connection) and keeps
  - the page's HTML after scripts ran source_snapshots/<facility>_<site>_<n>.html
  - a PDF made from that saved HTML   source_snapshots/<facility>_<site>_<n>.pdf
    (printing the live page a second time trips some sites' bot checks, so the PDF is rendered from the
    copy already captured; the verdict is judged on the HTML)
and records url, files, SHA-256 of the PDF, size, capture time (UTC) and a verdict in
source_snapshots/manifest.csv:
  ok       page captured and looks like the real page
  blocked  the site showed a bot check / access-denied page (open it by hand or use archive.ph)
  failed   Chrome could not load it (time-out, site down)
PDFs that the source itself serves (annual reports, parliament answers) are downloaded as they are.

The snapshots are third-party pages: keep source_snapshots/ OUT of the public repository (it is evidence
held by the authors, available on request). DATA.md says so.

Run from the project root:   python3 scripts/27_snapshot_sources.py          # the pages no web archive would take
                             python3 scripts/27_snapshot_sources.py --all    # every cited source URL
Data Center Map shows "Page View Limit Reached" after ~50 pages a day: those are marked blocked and are
retried on the next run (they are done last).
Resumable: pages already 'ok' are skipped. Standard library + Google Chrome only.
"""
import csv, hashlib, os, re, subprocess, sys, time, urllib.parse, urllib.request
from datetime import datetime, timezone

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
TODO = os.path.join(ROOT, 'data', 'public', 'archive_manual_todo.csv')
OUTD = os.path.join(ROOT, 'source_snapshots')
MAN = os.path.join(OUTD, 'manifest.csv')
FIELDS = ['url', 'facility_ids', 'pdf', 'html', 'sha256_pdf', 'bytes_pdf', 'sha256_html', 'captured_utc', 'verdict', 'note']
CHROME = next((p for p in ['/Applications/Google Chrome.app/Contents/MacOS/Google Chrome',
                           os.path.expanduser('~/Applications/Google Chrome.app/Contents/MacOS/Google Chrome'),
                           '/Applications/Chromium.app/Contents/MacOS/Chromium',
                           '/Applications/Microsoft Edge.app/Contents/MacOS/Microsoft Edge'] if os.path.exists(p)), None)
UA = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 14_0) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/129.0 Safari/537.36'
BLOCK = re.compile(r'just a moment|checking your browser|attention required|access denied|cf-chl|captcha|'
                   r'are you a robot|request unsuccessful|enable javascript and cookies|page view limit', re.I)
ALL = '--all' in sys.argv
if not CHROME: sys.exit('Google Chrome not found in /Applications; install it or edit CHROME at the top of this script.')
os.makedirs(OUTD, exist_ok=True)

PUB = os.path.join(ROOT, 'data', 'public')
if ALL:
    cites = {}
    for f, cols in [('inventory_sources_20260930.csv', ['source_url', 'source_url_2', 'mw_published_figures']), ('inventory_corrections_v2.csv', ['evidence_url']),   # 25 Oct 2026: capacity evidence too
                    ('inventory_issues_20260930.csv', ['evidence_url'])]:
        fp = os.path.join(PUB, f)
        if not os.path.exists(fp): continue
        for r in csv.DictReader(open(fp, encoding='utf-8')):
            for c in cols:
                for u in (r.get(c) or '').split():
                    u = u.strip().rstrip(';,')
                    if u.startswith('http') and 'web.archive.org' not in u:
                        cites.setdefault(u, set()).add(r.get('facility_id', ''))
    todo = [dict(url=u, facility_ids=' '.join(sorted(x for x in ids if x))) for u, ids in cites.items()]
    todo.sort(key=lambda r: 'datacentermap.com' in r['url'])          # Data Center Map last (daily page limit)
else:
    todo = [r for r in csv.DictReader(open(TODO, encoding='utf-8')) if not (r.get('archive_url_to_paste') or '').startswith('http')]
done = {r['url']: r for r in csv.DictReader(open(MAN, encoding='utf-8'))} if os.path.exists(MAN) else {}

def name_for(r, i):
    fac = (r.get('facility_ids') or 'X').split()[0]
    site = re.sub(r'[^a-z0-9]+', '-', urllib.parse.urlparse(r['url']).netloc.lower().replace('www.', ''))[:30]
    return os.path.join(OUTD, f'{fac}_{site}_{i:02d}')

def sha(p):
    h = hashlib.sha256(); h.update(open(p, 'rb').read()); return h.hexdigest()

def chrome(args, timeout=90):
    base = [CHROME, '--headless=new', '--disable-gpu', '--no-first-run', '--no-default-browser-check',
            f'--user-agent={UA}', '--virtual-time-budget=15000', '--run-all-compositor-stages-before-draw']
    return subprocess.run(base + args, capture_output=True, timeout=timeout)

def make_pdf(stem):
    """PDF of the saved HTML with its scripts removed (a page's own scripts can blank it when re-run offline)."""
    html = open(stem + '.html', encoding='utf-8', errors='ignore').read()
    tmp = stem + '.print.html'
    open(tmp, 'w', encoding='utf-8').write(re.sub(r'<script\b.*?</script>|<noscript\b.*?</noscript>', '', html, flags=re.S | re.I))
    try:
        chrome([f'--print-to-pdf={stem}.pdf', '--no-pdf-header-footer', 'file://' + urllib.parse.quote(tmp)])
    finally:
        try: os.remove(tmp)
        except OSError: pass
    return os.path.exists(stem + '.pdf') and os.path.getsize(stem + '.pdf') > 2000

def verdict_from_html(path):
    html = open(path, encoding='utf-8', errors='ignore').read()
    body = re.sub(r'<script.*?</script>|<style.*?</style>', ' ', html, flags=re.S | re.I)
    text = re.sub(r'\s+', ' ', re.sub(r'<[^>]+>', ' ', body)); n = len(text.split())
    if n < 80 or (n < 400 and BLOCK.search(text[:20000])):
        return 'blocked', f'bot check / page-limit / access page, or almost no text ({n} words)'
    t = re.search(r'<title[^>]*>(.*?)</title>', html, re.S | re.I)
    return 'ok', (t.group(1).strip()[:150] if t else f'{n} words')

def write():
    with open(MAN, 'w', newline='', encoding='utf-8') as f:
        w = csv.DictWriter(f, fieldnames=FIELDS); w.writeheader()
        for r in todo:
            if r['url'] in done: w.writerow(done[r['url']])

# Repair pass on earlier captures (no new downloads): re-judge saved HTML with the current rule, and make
# any missing PDF from the saved HTML.
fixed = 0
for u, rec in done.items():
    if not rec.get('html') or not os.path.exists(os.path.join(OUTD, rec['html'])): continue
    stem = os.path.join(OUTD, rec['html'][:-5])
    if rec.get('verdict') == 'blocked':
        v, note = verdict_from_html(stem + '.html')
        if v == 'ok': rec.update(verdict='ok', note=note); fixed += 1
    if rec.get('verdict') == 'ok' and not (rec.get('pdf') and os.path.getsize(os.path.join(OUTD, rec['pdf'])) > 2000):
        if make_pdf(stem):
            rec.update(pdf=os.path.basename(stem + '.pdf'), sha256_pdf=sha(stem + '.pdf'), bytes_pdf=os.path.getsize(stem + '.pdf')); fixed += 1
if fixed: print(f'repaired {fixed} earlier captures (verdict re-judged or PDF re-made from the saved HTML)'); write()

ARCH = os.path.join(ROOT, 'data', 'public', 'source_archive.csv')
archived = {r['url'] for r in csv.DictReader(open(ARCH, encoding='utf-8'))
            if r.get('method') in ('existing', 'saved', 'existing_older', 'saved_manual')} if os.path.exists(ARCH) else set()
jobs = [r for r in todo if done.get(r['url'], {}).get('verdict') != 'ok']
# Data Center Map allows ~50 page views a day: spend them first on pages with no web-archive copy
jobs.sort(key=lambda r: ('datacentermap.com' in r['url'], r['url'] in archived))
print(f'{len(todo)} pages without an archive link; {len(todo) - len(jobs)} already snapshotted; {len(jobs)} to do')

dcm_misses = 0   # consecutive Data Center Map pages that did not come back 'ok'
for i, r in enumerate(jobs, 1):
    u = r['url']; stem = name_for(r, todo.index(r) + 1)
    if 'datacentermap.com' in u and dcm_misses >= 3:
        # the daily page-view limit is spent: stop instead of hammering the site (pages left are retried next run)
        print(f'Data Center Map stopped answering after 3 misses in a row (daily page limit); '
              f'{len(jobs) - i + 1} DCM pages left for the next run'); break
    rec = dict(url=u, facility_ids=r.get('facility_ids', ''), pdf='', html='', sha256_pdf='', bytes_pdf='', sha256_html='',
               captured_utc=datetime.now(timezone.utc).strftime('%Y-%m-%d %H:%M'), verdict='failed', note='')
    try:
        if u.lower().split('?')[0].endswith('.pdf'):          # the source is itself a PDF: keep the original file
            req = urllib.request.Request(u, headers={'User-Agent': UA})
            data = urllib.request.urlopen(req, timeout=90).read()
            if data[:4] == b'%PDF':
                open(stem + '.pdf', 'wb').write(data); rec.update(pdf=os.path.basename(stem + '.pdf'), verdict='ok', note='original PDF')
            else:
                rec['note'] = 'not a PDF (blocked or moved)'; rec['verdict'] = 'blocked'
        else:
            h = chrome(['--dump-dom', u])
            html = h.stdout.decode('utf-8', 'ignore')
            body = re.sub(r'<script.*?</script>|<style.*?</style>', ' ', html, flags=re.S | re.I)
            text = re.sub(r'\s+', ' ', re.sub(r'<[^>]+>', ' ', body))
            if html.strip():
                # a <base> tag lets the saved copy load its own stylesheets and images when printed
                if '<head>' in html and '<base ' not in html: html = html.replace('<head>', f'<head><base href="{u}">', 1)
                open(stem + '.html', 'w', encoding='utf-8').write(html); rec['html'] = os.path.basename(stem + '.html')
                if make_pdf(stem): rec['pdf'] = os.path.basename(stem + '.pdf')
            nwords = len(text.split())
            if not html.strip():
                rec['note'] = 'Chrome returned no page'
            elif nwords < 80 or (nwords < 400 and BLOCK.search(text[:20000])):
                rec.update(verdict='blocked', note=f'bot check / page-limit / access page, or almost no text ({nwords} words)')
            else:
                t = re.search(r'<title[^>]*>(.*?)</title>', html, re.S | re.I)
                rec.update(verdict='ok', note=(t.group(1).strip()[:150] if t else f'{nwords} words'))
        if rec['html']: rec['sha256_html'] = sha(os.path.join(OUTD, rec['html']))
        if rec['pdf']:
            pp = os.path.join(OUTD, rec['pdf']); rec.update(sha256_pdf=sha(pp), bytes_pdf=os.path.getsize(pp))
    except subprocess.TimeoutExpired:
        rec['note'] = 'time-out'
    except Exception as e:
        rec['note'] = f'{type(e).__name__}: {e}'[:200]
    if 'datacentermap.com' in u:
        dcm_misses = 0 if rec['verdict'] == 'ok' else dcm_misses + 1
    done[u] = rec
    print(f"[{i}/{len(jobs)}] {rec['verdict']:7s} {u[:95]}", flush=True)
    write(); time.sleep(3)

write()
v = {}
for r in todo:
    k = done.get(r['url'], {}).get('verdict', 'not run'); v[k] = v.get(k, 0) + 1
print('summary:', ', '.join(f'{k} {n}' for k, n in sorted(v.items())), f'-> {MAN}')
print("Open the PDFs marked 'blocked' or 'failed' by hand (or use archive.ph); look at a few 'ok' ones too.")
