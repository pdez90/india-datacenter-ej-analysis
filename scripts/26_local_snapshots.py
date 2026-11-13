#!/usr/bin/env python3
"""26_local_snapshots.py -- keep a copy of every source the Internet Archive could not save.

For each URL in data/public/source_archive.csv whose method starts with "failed" (Data Center Map
excluded; it is handled by hand), this script
  1. downloads the page from this Mac with a normal browser User-Agent and saves it under
     data/public/source_snapshots/ (PDFs as .pdf, pages as .html), recording date, HTTP status,
     size and SHA-256 in data/public/source_snapshots.csv;
  2. adds the URL to data/public/archive_manual_todo.csv with an archive.ph save link, so a public
     copy can be made by hand (open save_link, paste the archive.ph result into archive_url_to_paste;
     25_archive_sources.py then counts it as archived).
Resumable: URLs already saved locally or already in the to-do file are skipped.
Standard library only. Run from the project root:
    python3 scripts/26_local_snapshots.py
"""
import csv, hashlib, os, re, time, urllib.parse, urllib.request
from datetime import datetime, timezone

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
PUB = os.path.join(ROOT, 'data', 'public')
ARCH = os.path.join(PUB, 'source_archive.csv')
SNAPDIR = os.path.join(PUB, 'source_snapshots')
SNAPLOG = os.path.join(PUB, 'source_snapshots.csv')
TODO = os.path.join(PUB, 'archive_manual_todo.csv')
UA = {'User-Agent': 'Mozilla/5.0 (Macintosh; Intel Mac OS X 14_0) AppleWebKit/537.36 (KHTML, like Gecko) '
                    'Chrome/126.0 Safari/537.36',
      'Accept': 'text/html,application/pdf,*/*;q=0.8', 'Accept-Language': 'en-IN,en;q=0.9'}
os.makedirs(SNAPDIR, exist_ok=True)

failed = [r for r in csv.DictReader(open(ARCH, encoding='utf-8'))
          if r['method'].startswith('failed') and 'datacentermap.com' not in r['url']]
print(f'{len(failed)} sources the Internet Archive could not save')

# facility ids per URL, for the to-do file
fac = {}
src = os.path.join(PUB, 'inventory_sources_20260930.csv')
if os.path.exists(src):
    for r in csv.DictReader(open(src, encoding='utf-8')):
        for c in ('source_url', 'source_url_2'):
            for u in (r.get(c) or '').split():
                fac.setdefault(u.strip().rstrip(';,'), []).append(r.get('facility_id') or r.get('id') or '')

# ---- 1. local copies -----------------------------------------------------------------------------
log = {}
if os.path.exists(SNAPLOG):
    for r in csv.DictReader(open(SNAPLOG, encoding='utf-8')): log[r['url']] = r

def slug(u):
    p = urllib.parse.urlparse(u)
    s = re.sub(r'[^A-Za-z0-9]+', '_', (p.netloc + p.path).strip('/'))[:110]
    return s + '_' + hashlib.sha1(u.encode()).hexdigest()[:8]

for i, r in enumerate(failed, 1):
    u = r['url']
    if log.get(u, {}).get('status') == '200':
        print(f'[{i}/{len(failed)}] already saved  {u[:90]}'); continue
    rec = dict(url=u, file='', status='', bytes='', sha256='', saved_at=datetime.now(timezone.utc).strftime('%Y-%m-%d %H:%M'))
    try:
        with urllib.request.urlopen(urllib.request.Request(u, headers=UA), timeout=60) as resp:
            body = resp.read(); ctype = resp.headers.get('Content-Type', '')
            ext = '.pdf' if ('pdf' in ctype or u.lower().endswith('.pdf')) else '.html'
            fn = slug(u) + ext
            open(os.path.join(SNAPDIR, fn), 'wb').write(body)
            rec.update(file=f'source_snapshots/{fn}', status=str(resp.status), bytes=str(len(body)),
                       sha256=hashlib.sha256(body).hexdigest())
    except Exception as e:
        rec['status'] = getattr(e, 'code', None) and str(e.code) or type(e).__name__
    log[u] = rec
    print(f'[{i}/{len(failed)}] {rec["status"]:>4} {rec["bytes"]:>8} B  {u[:90]}')
    time.sleep(2)

with open(SNAPLOG, 'w', newline='', encoding='utf-8') as f:
    w = csv.DictWriter(f, fieldnames=['url', 'file', 'status', 'bytes', 'sha256', 'saved_at'])
    w.writeheader(); w.writerows(log.values())

# ---- 2. archive.ph to-do -------------------------------------------------------------------------
cols = ['url', 'why_failed', 'facility_ids', 'save_link', 'archive_url_to_paste']
todo = list(csv.DictReader(open(TODO, encoding='utf-8'))) if os.path.exists(TODO) else []
have = {r['url'] for r in todo}
added = 0
for r in failed:
    if r['url'] in have: continue
    todo.append(dict(url=r['url'], why_failed=r['method'], facility_ids=' '.join(sorted(set(fac.get(r['url'], [])))),
                     save_link='https://archive.ph/?url=' + urllib.parse.quote(r['url'], safe=''), archive_url_to_paste=''))
    added += 1
with open(TODO, 'w', newline='', encoding='utf-8') as f:
    w = csv.DictWriter(f, fieldnames=cols, extrasaction='ignore'); w.writeheader(); w.writerows(todo)

ok = sum(1 for r in failed if log.get(r['url'], {}).get('status') == '200')
print(f'\nlocal copies: {ok}/{len(failed)} saved in {SNAPDIR} (log: {SNAPLOG})')
print(f'archive.ph to-do: {added} added, {len(todo)} rows in {TODO}')
print('Small .html files (a few kB) are usually a block page, not the source: check them, and use archive.ph for those.')
