#!/usr/bin/env python3
"""26_check_links.py -- check that every source URL in the public table is live and is about the right facility.

For each facility row in outputs/public/india_datacentres_public.csv, and for both source_url and
source_url_2, this script opens the page (as a normal browser would) and records:
  http_status      200 = page loads; 404/410 = gone; 403/429/503 = the site blocks automated checks
  final_url        where the link ends up after redirects (a redirect to a home page is a warning sign)
  page_title       the page's <title>
  operator_on_page whether a distinctive word of the operator's name appears on the page
  city_on_page     whether the city appears on the page
  verdict          ok | check (loads, but operator or city not found on the page) | dead | blocked | error
Results go to data/public/source_linkcheck.csv (one line per facility x source). The script is resumable.
Pages that block automated checks (Data Center Map does) are marked `blocked`: open a few by hand.

Run from the project root:   python3 scripts/26_check_links.py
Standard library only. It waits ~1.5 s between requests and never sends anything but a page request.
"""
import csv, gzip, html, os, re, sys, time, urllib.error, urllib.request, zlib
from datetime import datetime, timezone

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
PUB_TABLE = os.path.join(ROOT, 'outputs', 'public', 'india_datacentres_public.csv')
OUT = os.path.join(ROOT, 'data', 'public', 'source_linkcheck.csv')
UA = {'User-Agent': 'Mozilla/5.0 (Macintosh; Intel Mac OS X 14_0) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/126.0 Safari/537.36',
      'Accept': 'text/html,application/xhtml+xml,application/pdf;q=0.9,*/*;q=0.8', 'Accept-Language': 'en'}
STOP = {'data', 'center', 'centre', 'centers', 'centres', 'india', 'private', 'limited', 'technologies', 'services',
        'solutions', 'global', 'cloud', 'infra', 'infrastructure', 'group', 'by', 'and', 'the', 'of', 'ltd', 'pvt', 'inc',
        'national', 'state', 'government', 'department', 'information', 'technology'}
FIELDS = ['facility_id', 'which', 'url', 'http_status', 'final_url', 'page_title', 'operator_on_page', 'city_on_page',
          'verdict', 'checked_at']

def words(s, n=4):
    return [w for w in re.findall(r'[a-z0-9]+', (s or '').lower()) if len(w) >= n and w not in STOP]

def fetch(url):
    req = urllib.request.Request(url, headers=UA)
    with urllib.request.urlopen(req, timeout=40) as r:
        raw = r.read(3_000_000); enc = (r.headers.get('Content-Encoding') or '').lower()
        if enc == 'gzip': raw = gzip.decompress(raw)
        elif enc == 'deflate': raw = zlib.decompress(raw)
        ctype = r.headers.get('Content-Type', '')
        return r.status, r.geturl(), ctype, raw

rows = list(csv.DictReader(open(PUB_TABLE, encoding='utf-8')))
done = {}
if os.path.exists(OUT):
    for r in csv.DictReader(open(OUT, encoding='utf-8')): done[(r['facility_id'], r['which'])] = r
jobs = [(r, w) for r in rows for w in ('source_url', 'source_url_2') if (r.get(w) or '').startswith('http')]
todo = [(r, w) for r, w in jobs if done.get((r['facility_id'], w), {}).get('verdict') not in ('ok', 'check', 'dead', 'blocked')]
print(f'{len(jobs)} facility-source links; {len(jobs) - len(todo)} already checked; {len(todo)} to do')

def write():
    with open(OUT, 'w', newline='', encoding='utf-8') as f:
        w = csv.DictWriter(f, fieldnames=FIELDS); w.writeheader()
        for r, wh in jobs:
            if (r['facility_id'], wh) in done: w.writerow(done[(r['facility_id'], wh)])

cache = {}
for i, (r, which) in enumerate(todo, 1):
    url = r[which].split()[0]
    rec = dict(facility_id=r['facility_id'], which=which, url=url, http_status='', final_url='', page_title='',
               operator_on_page='', city_on_page='', verdict='error', checked_at=datetime.now(timezone.utc).strftime('%Y-%m-%d %H:%M'))
    try:
        if url not in cache:
            cache[url] = fetch(url); time.sleep(1.5)
        status, final, ctype, raw = cache[url]
        text = raw.decode('utf-8', 'ignore') if 'pdf' not in ctype.lower() else raw.decode('latin-1', 'ignore')
        title = re.search(r'<title[^>]*>(.*?)</title>', text, re.S | re.I)
        low = html.unescape(re.sub(r'<[^>]+>', ' ', text)).lower()
        op_words = words(r['operator']) or words(r['facility_name'])
        city_words = words(r['city'].split(',')[0], 4) if r.get('city') else []
        op_hit = any(w in low for w in op_words) if op_words else None
        city_hit = any(w in low for w in city_words) if city_words else None
        rec.update(http_status=status, final_url=final, page_title=(html.unescape(title.group(1)).strip()[:150] if title else ''),
                   operator_on_page=op_hit, city_on_page=city_hit,
                   verdict='ok' if (op_hit is not False) else 'check')
        if 'pdf' in ctype.lower(): rec['page_title'] = rec['page_title'] or '(PDF)'
    except urllib.error.HTTPError as e:
        rec.update(http_status=e.code, verdict='dead' if e.code in (404, 410) else 'blocked' if e.code in (401, 403, 429, 451, 503, 520, 521, 522, 523) else 'error')
    except Exception as e:
        rec.update(http_status=type(e).__name__, verdict='error')
    done[(r['facility_id'], which)] = rec
    print(f"[{i}/{len(todo)}] {rec['verdict']:7s} {str(rec['http_status']):4s} {r['facility_id']} {url[:85]}")
    if i % 10 == 0: write()
write()
v = {}
for rec in done.values(): v[rec['verdict']] = v.get(rec['verdict'], 0) + 1
print('summary:', ', '.join(f'{k} {n}' for k, n in sorted(v.items())), f'-> {OUT}')
print('Look at verdict = check, dead and error rows; open a sample of blocked ones by hand.')
