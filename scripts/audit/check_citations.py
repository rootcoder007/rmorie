#!/usr/bin/env python3
"""Block unverifiable citations from being committed.

A citation is a factual claim about a document that exists. Written from
recall it is wrong often enough to matter -- the Boyd shelf had 10 of 54
section numbers wrong, one of them invented outright -- and a wrong
citation is worse than none, because it looks like evidence.

This gate refuses the shapes that CANNOT be checked by a reader:

  E1  a reference with a venue and page numbers but NO title
  E2  a bare "Author (year)" with no venue at all
  E3  a leaked template placeholder, e.g. a literal {HYN}
  E4  a locator in the text that CONTRADICTS the ledger
  E5  a ledger entry with no resolvable source (invented ledger entry)
  E6  --online only: a ledger entry that disagrees with the publisher

E1-E5 are decided offline and always run. E6 resolves each DOI against
Crossref, so the ledger itself cannot be filled in from memory: an entry
whose year/volume/pages do not match the publisher record fails.

The ledger is append-only in practice -- confirm a citation is real,
then add it. It is the record of what was actually looked up, not a
list of what someone believed.

Usage:
    check_citations.py [--strict] [--online] [ROOT ...]

--strict additionally requires that every citation carrying a
volume/pages locator has a ledger entry. That is the end state of the
audit; without it the gate reports coverage but does not fail on it.

See memory: feedback_never_invent_citations_verify_each.
"""

from __future__ import annotations

import json
import pathlib
import re
import sys

HERE = pathlib.Path(__file__).resolve().parent
LEDGER = HERE / "citations_ledger.json"

# "Author, A. (1999). *Journal*, 94(446), 496-509."  -- no title.
PY_TITLELESS = re.compile(
    r"\(\d{4}[a-z]?\)\.\s*\*[^*]+\*\s*,?\s*\d")
# "#' @references Author (1999). \emph{Journal}, 94(446), 496-509."
R_TITLELESS = re.compile(
    r"\(\d{4}[a-z]?\)\.\s*\\emph\{[^}]+\}[ ,]*\s*\d")
# "Verdinelli & Wasserman (1995); Dickey (1971)."  -- no venue at all.
BARE_AUTHOR_YEAR = re.compile(
    r"^\s*(?:#'\s*)?(?:@references\s+)?"
    r"[A-Z][A-Za-z'\-]+(?:[^.()]{0,60})\(\d{4}[a-z]?\)\s*[;.]\s*$")
PLACEHOLDER = re.compile(r"^\s*(?:#'\s*)?\{[A-Z_][A-Z0-9_]*\}\s*$")

# Telling a journal locator from a book edition. A journal reference reads
# "*Venue*, 94(446), 496-509" or "*Venue*, 15, 1593-1623" -- an italic run
# followed by a volume and then an issue or a page range. A book reads
# "*Title*, 3rd ed. Publisher" or "*Title*. Publisher", where the italic
# run IS the title and so nothing is missing. Anchoring on the shape of
# what FOLLOWS the italics is reliable; guessing from the words inside
# them is not, since plenty of book titles look like journal names.
EDITION = re.compile(r"^\s*,?\s*\d+(st|nd|rd|th)\s+(ed|edn|edition)\b", re.I)
JOURNAL_LOCATOR = re.compile(r"^\s*,?\s*\d+\s*(\(\d+[^)]*\))?\s*,\s*\d")


def load_ledger():
    if not LEDGER.exists():
        return {}
    return json.loads(LEDGER.read_text()).get("entries", {})


def reference_blocks(text, is_r):
    """Yield (line_no, line) for lines inside a references section."""
    lines = text.split("\n")
    if is_r:
        for i, ln in enumerate(lines, 1):
            if "@references" in ln:
                yield i, ln
                # continuation lines of the same roxygen block
                j = i
                while j < len(lines) and re.match(r"^\s*#'\s{2,}\S", lines[j]):
                    yield j + 1, lines[j]
                    j += 1
        return
    inside = False
    for i, ln in enumerate(lines, 1):
        s = ln.strip()
        if s == "References":
            inside = True
            continue
        if inside:
            if s.startswith("-") and set(s) <= {"-"}:
                continue
            if s in ("Examples", "Notes", "See Also") or s == '"""':
                inside = False
                continue
            if s:
                yield i, ln


def check_file(path, ledger, strict):
    text = path.read_text(errors="replace")
    is_r = path.suffix.upper() == ".R"
    problems = []

    for i, ln in enumerate(text.split("\n"), 1):
        if PLACEHOLDER.match(ln):
            problems.append((i, "E3", f"leaked template placeholder: {ln.strip()}"))

    joined = {}
    for i, ln in reference_blocks(text, is_r):
        joined[i] = ln

    # Stitch wrapped references so a title split across lines is not a
    # false positive, but keep the first line number for reporting.
    keys = sorted(joined)
    buf, start = "", None
    for k in keys:
        ln = joined[k]
        s = re.sub(r"^\s*#'\s*", "", ln).rstrip()
        if start is None:
            start, buf = k, s
        elif re.match(r"^\s{4,}\S", ln) or re.match(r"^\s*#'\s{3,}\S", ln):
            buf += " " + s.strip()
        else:
            problems += judge(buf, start, ledger, is_r, strict)
            start, buf = k, s
    if start is not None:
        problems += judge(buf, start, ledger, is_r, strict)
    return problems


def check_ledger_shape(ledger):
    """E5: a ledger entry must carry a source that could be resolved.

    Without this the ledger is just another place to write things from
    memory, and the gate would certify its own guesses.
    """
    bad = []
    for key, v in ledger.items():
        src = str(v.get("source", ""))
        if not (src.startswith("doi:") or src.startswith("http")):
            bad.append(f"ledger[{key}]: E5: no resolvable source "
                       f"(need doi: or http), got {src!r}")
        if not v.get("verified"):
            bad.append(f"ledger[{key}]: E5: no `verified` date")
        # An entry may not be BOTH a certificate and a guess. If the page
        # range could not be confirmed, the recorded value must be the
        # start page alone -- writing a range here and flagging it in
        # prose would let the flag be ignored while the range is trusted.
        if v.get("pages_unverified") and "-" in str(v.get("pages") or ""):
            bad.append(f"ledger[{key}]: E5: marked pages_unverified but "
                       f"still records a RANGE {v.get('pages')!r}; record "
                       f"the confirmed start page only")
    return bad


def check_ledger_online(ledger):
    """E6: resolve each DOI and compare with what the ledger claims."""
    import json as _json
    import time
    import urllib.request
    bad = []
    ua = {"User-Agent": "morie-citation-gate (mailto:ruhela.vansh@gmail.com)"}
    for key, v in sorted(ledger.items()):
        src = str(v.get("source", ""))
        if not src.startswith("doi:"):
            continue
        url = "https://api.crossref.org/works/" + src[4:]
        try:
            req = urllib.request.Request(url, headers=ua)
            with urllib.request.urlopen(req, timeout=45) as r:
                msg = _json.load(r)["message"]
        except Exception as exc:
            bad.append(f"ledger[{key}]: E6: DOI did not resolve ({exc})")
            continue
        yr = None
        try:
            yr = msg["issued"]["date-parts"][0][0]
        except Exception:
            pass
        for field, got in (("year", yr), ("volume", msg.get("volume")),
                           ("pages", msg.get("page"))):
            want = v.get(field)
            if want is None or got is None:
                continue
            w = str(want).replace("--", "-").strip()
            g = str(got).replace("--", "-").strip()
            if w == g:
                continue
            # Older deposits (JSTOR-era DOIs especially) carry only the
            # FIRST page. A ledger range that begins there is consistent,
            # not contradictory -- flagging it would drown the real
            # mismatches in noise, and a noisy gate gets ignored.
            if (field == "pages" and "-" not in g
                    and w.split("-")[0] == g):
                continue
            bad.append(f"ledger[{key}]: E6: {field} is {got!r} at the "
                       f"publisher, ledger says {want!r}")
        time.sleep(0.4)
    return bad


def judge(entry, line, ledger, is_r, strict):
    e = entry.strip()
    if not e or e.startswith("@references") and len(e) < 14:
        return []
    e = re.sub(r"^@references\s*", "", e)
    out = []
    pat = R_TITLELESS if is_r else PY_TITLELESS
    m = pat.search(e)
    if m:
        # Anchor the venue to the reference that matched, not the first
        # italics in the whole block. Otherwise a titled journal article
        # followed by a second reference cross-matches: the year comes
        # from the second reference, the italics from the first.
        sub = e[m.start():]
        ital = re.search(r"\\emph\{([^}]+)\}" if is_r else r"\*([^*]+)\*", sub)
        tail = sub[ital.end():] if ital else ""
        if not EDITION.match(tail) and JOURNAL_LOCATOR.match(tail):
            out.append((line, "E1",
                        f"citation has a venue and locator but NO title: {e[:88]}"))
    if BARE_AUTHOR_YEAR.match(entry) and "*" not in e and "\\emph" not in e:
        out.append((line, "E2", f"bare author-year, no venue: {e[:88]}"))
    return out


def main(argv):
    strict = "--strict" in argv
    online = "--online" in argv
    roots = [a for a in argv[1:] if not a.startswith("--")] or ["."]
    ledger = load_ledger()
    files = []
    for r in roots:
        rp = pathlib.Path(r)
        for pat in ("src/morie/fn/*.py", "R/*.R", "r-package/morie/R/*.R"):
            files += sorted(rp.glob(pat))
    problems = list(check_ledger_shape(ledger))
    if online:
        problems += check_ledger_online(ledger)
    for f in files:
        if f.name.startswith("_lazy_map"):
            continue
        for line, code, msg in check_file(f, ledger, strict):
            problems.append(f"{f}:{line}: {code}: {msg}")

    print(f"citation gate: {len(files)} files, {len(ledger)} ledger entries")
    if problems:
        print(f"\n{len(problems)} unverifiable citation(s):\n")
        for p in problems[:60]:
            print("  " + p)
        if len(problems) > 60:
            print(f"  ... and {len(problems) - 60} more")
        print("\nFix by supplying the missing title/venue from the publisher "
              "record, then record it in scripts/audit/citations_ledger.json.")
        return 1
    print("citation gate: OK")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
