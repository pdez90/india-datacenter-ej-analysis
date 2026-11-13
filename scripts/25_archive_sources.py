#!/usr/bin/env python3
"""25_archive_sources.py -- preserve every source URL of the public inventory in the Internet Archive.

For each unique URL cited in data/public/ (facility sources, correction evidence, open-issue evidence),
this script
  1. asks the Wayback Machine whether a snapshot from on/after 1 Sep 2026 already exists; if so it is used;
  2. otherwise asks Save Page Now (https://web.archive.org/save/<url>) to take a snapshot now;
  3. if the site refuses archiving (some Cloudflare-protected sites return HTTP 520), it records the newest
     older snapshot instead (method = existing_older; its date is in `snapshot`). After three refusals
     from one site it stops asking Save Page Now for that site.
Results go to data/public/source_archive.csv (url, archive_url, snapshot, method, checked_at). The script
is resumable: URLs already archived are skipped, so re-run it until nothing is left to do.
Save Page Now limits anonymous use to a few captures a minute, so a full first run takes about 1-2 hours.
24_public_inventory.R then adds source_url_archive / source_url_2_archive columns to the public table.

Run from the project root on a machine with normal internet access:
    python3 scripts/25_archive_sources.py            # all URLs
    python3 scripts/25_archive_sources.py --limit 50 # a batch
Standard library only.
"""
import re, csv, json, os, sys, time, urllib.parse, urllib.request, urllib.error
from datetime import datetime, timezone

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
PUB = os.path.join(ROOT, 'data', 'public')
OUT = os.path.join(PUB, 'source_archive.csv')
SINCE = '20260901'
UA = {'User-Agent': 'india-datacentre-ej source archiver (research; contact via github.com/pdez90/india-datacentre-ej)'}
limit = int(sys.argv[sys.argv.index('--limit') + 1]) if '--limit' in sys.argv else None
# --retry-failed: retry only pages recorded as failed, skip Data Center Map (it blocks the archive),
# ignore the per-site give-up rule, and wait longer between captures.
RETRY = '--retry-failed' in sys.argv

def urls_in(path, cols):
    if not os.path.exists(path): return []
    out = []
    for r in csv.DictReader(open(path, encoding='utf-8')):
        for c in cols:
            for u in (r.get(c) or '').split():
                if u.startswith('http'): out.append(u.strip().rstrip(';,'))
    return out

urls = []
urls += urls_in(os.path.join(PUB, 'inventory_sources_20260930.csv'), ['source_url', 'source_url_2', 'mw_published_figures'])   # 25 Oct 2026: capacity evidence URLs too
urls += urls_in(os.path.join(PUB, 'inventory_corrections_v2.csv'), ['evidence_url'])
urls += urls_in(os.path.join(PUB, 'inventory_issues_20260930.csv'), ['evidence_url'])
urls = list(dict.fromkeys(u for u in urls if 'web.archive.org' not in u))

done = {}
if os.path.exists(OUT):
    for r in csv.DictReader(open(OUT, encoding='utf-8')): done[r['url']] = r
# Copies made by hand (data/public/archive_manual_todo.csv, column archive_url_to_paste: a
# web.archive.org/web/... or archive.ph/... link) are taken as archived, method = saved_manual.
MANUAL = os.path.join(PUB, 'archive_manual_todo.csv')
if os.path.exists(MANUAL):
    for r in csv.DictReader(open(MANUAL, encoding='utf-8')):
        a = (r.get('archive_url_to_paste') or '').strip()
        if r.get('url') in urls and a.startswith('http'):
            ts = a.split('/web/')[1].split('/')[0] if '/web/' in a else ''.join(re.findall(r'/(\d{4})\.(\d{2})\.(\d{2})-(\d{6})/', a)[0]) if re.search(r'/\d{4}\.\d{2}\.\d{2}-\d{6}/', a) else ''
            done[r['url']] = dict(url=r['url'], archive_url=a, snapshot=ts, method='saved_manual',
                                  checked_at=datetime.now(timezone.utc).strftime('%Y-%m-%d %H:%M'))
todo = [u for u in urls if done.get(u, {}).get('method') not in ('existing', 'saved', 'existing_older', 'saved_manual')]
if RETRY: todo = [u for u in todo if 'datacentermap.com' not in u]
print(f'{len(urls)} unique URLs; {len(urls) - len(todo)} already archived; {len(todo)} to do')
if limit: todo = todo[:limit]

def get(url, timeout=90):
    req = urllib.request.Request(url, headers=UA)
    with urllib.request.urlopen(req, timeout=timeout) as r:
        return r.status, r.geturl(), dict(r.headers), r.read()

BUSY = {'streak': 0}   # consecutive look-ups the archive refused (503/429/time-out)

def _lookup_cdx(u):
    q = 'https://web.archive.org/cdx/search/cdx?' + urllib.parse.urlencode(
        {'url': u, 'output': 'json', 'filter': 'statuscode:200', 'fl': 'timestamp,original', 'limit': '-1'})
    _, _, _, body = get(q, 60)
    rows = json.loads(body) if body.strip() else []
    if len(rows) > 1:
        ts, orig = rows[-1][0], rows[-1][1]
        return f'https://web.archive.org/web/{ts}/{orig}', ts
    return None, None

def _lookup_available(u):   # second, separate archive service, used when the CDX index is overloaded
    q = 'https://archive.org/wayback/available?' + urllib.parse.urlencode({'url': u, 'timestamp': '20260930'})
    _, _, _, body = get(q, 60)
    s = json.loads(body).get('archived_snapshots', {}).get('closest')
    if s and s.get('available'): return s['url'], s['timestamp']
    return None, None

def available(u):
    """Newest existing Wayback snapshot: (url, timestamp, is_recent, checked_ok).
    When the archive is overloaded (HTTP 503/429 or time-outs) it tries the second look-up service, then
    waits progressively longer and retries, instead of recording the page as failed."""
    for wait in (0, 60, 180, 300):
        if wait:
            print(f'  archive overloaded; waiting {wait} s before retrying'); time.sleep(wait)
        for fn in (_lookup_cdx, _lookup_available):
            try:
                arch, ts = fn(u)
                time.sleep(3)                               # pace look-ups
                BUSY['streak'] = 0
                return arch, ts, bool(ts and ts >= SINCE), True
            except urllib.error.HTTPError as e:
                if e.code not in (429, 502, 503, 504): print('  index look-up failed:', e); return None, None, False, False
            except Exception:
                pass                                        # time-out / connection reset: treat as busy
    BUSY['streak'] += 1
    if BUSY['streak'] >= 3:
        print('  the archive has been unavailable for several pages in a row; pausing 15 minutes')
        time.sleep(900); BUSY['streak'] = 0
    return None, None, False, False

def save(u):
    for attempt in range(3):
        try:
            status, final, headers, _ = get('https://web.archive.org/save/' + u, 180)
            loc = headers.get('Content-Location') or headers.get('content-location')
            arch = ('https://web.archive.org' + loc) if loc else (final if '/web/' in final else '')
            if arch:
                ts = arch.split('/web/')[1].split('/')[0] if '/web/' in arch else ''
                return arch, ts, 'saved'
        except urllib.error.HTTPError as e:
            if e.code in (429, 502, 503, 504):
                print(f'  archive busy (HTTP {e.code}); waiting 90 s'); time.sleep(90); continue
            return '', '', f'failed (HTTP {e.code})'
        except Exception as e:
            print('  save error:', e); time.sleep(20)
    return '', '', 'failed'

def write():
    rows = [done[u] for u in urls if u in done]
    with open(OUT, 'w', newline='', encoding='utf-8') as f:
        w = csv.DictWriter(f, fieldnames=['url', 'archive_url', 'snapshot', 'method', 'checked_at']); w.writeheader(); w.writerows(rows)

blocked = {}   # domains that refuse Save Page Now (e.g. HTTP 520 from Cloudflare-protected sites)
def domain(u):
    d = urllib.parse.urlparse(u).netloc.lower()
    return d[4:] if d.startswith('www.') else d
for i, u in enumerate(todo, 1):
    old_arch, old_ts, recent, checked = available(u)
    if old_arch and recent:
        arch, ts, method = old_arch, old_ts, 'existing'
    else:
        if not RETRY and blocked.get(domain(u), 0) >= 3 and checked:
            arch, ts, method = '', '', 'failed (site blocks archiving)'
        else:
            arch, ts, method = save(u); time.sleep(20 if RETRY else 10)  # stay under Save Page Now's anonymous rate limit
            blocked[domain(u)] = blocked.get(domain(u), 0) + 1 if method.startswith('failed') else 0
        if method.startswith('failed') and old_arch:   # fall back to the newest older snapshot
            arch, ts, method = old_arch, old_ts, 'existing_older'
        if method.startswith('failed') and not checked:
            method = 'failed (archive busy; re-run)'
    done[u] = dict(url=u, archive_url=arch, snapshot=ts, method=method, checked_at=datetime.now(timezone.utc).strftime('%Y-%m-%d %H:%M'))
    print(f'[{i}/{len(todo)}] {method:9s} {u[:90]}')
    if i % 10 == 0: write()
write()
ok = sum(1 for u in urls if done.get(u, {}).get('method') in ('existing', 'saved', 'saved_manual'))
old = sum(1 for u in urls if done.get(u, {}).get('method') == 'existing_older')
print(f'archived {ok}/{len(urls)} (snapshot from Sep 2026 or later); {old} more with an older snapshot only; failures are listed in {OUT} (method = failed...) - re-run to retry them')
