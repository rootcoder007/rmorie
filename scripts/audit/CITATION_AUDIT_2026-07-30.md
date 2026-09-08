# Citation audit — 386 modules, 368 unique citations

Triggered 2026-07-30 after the Boyd shelf was found to have been written
from recall, with 10 of 54 section citations wrong. Vee asked for the
same check on everything done recently, and for it to be done by hand,
one citation at a time, rather than by a similarity script.

**Scope**: every `src/morie/fn/*.py` module touched between `fdbf4ef975`
and `b6ad177b81` — the Wasserman, ESL, and batch100 shelves. 386 modules,
459 citation entries, 368 unique.

**Method**: each citation read individually and checked against the
publisher record (Crossref / OpenAlex by DOI or bibliographic query), the
authors' own hosted copy where one exists, or the local reference corpus
for books. A fuzzy-match script was written first and **abandoned** — it
returned 199 meaningless "NO-MATCH" verdicts for books, which Crossref
simply does not index, and would have let real errors through as noise.

## Confirmed defects and fixes

| # | Module(s) | Defect | Correction |
|---|---|---|---|
| 1 | `dpadam` | Cited `*CCS 2016*, 308-318` with **no paper title** | Abadi et al., "Deep learning with differential privacy". Venue/pages were right |
| 2 | `fgsbh` | Cited `*JASA*, 94(446), 496-509` with **no title** | Fine & Gray, "A proportional hazards model for the subdistribution of a competing risk" |
| 3 | 13 DP modules | Dwork & Roth 2014 pages **211-407** | **211-487** (publisher record). `acwhe compdp dpamp dpepsm dpexpm dpglap dpkmn dplog dpmed dprnyi dprrep dpsgd dpunit` |
| 4 | `acfP`, `esttsl` | Literal `{HYN}` template placeholder left in the rendered docstring | Removed |
| 5 | `aftres` | Cox & Snell 1968 as `30(2), 248-275` | **248-265** — 248-275 spans the discussion, not the article |
| 6 | `breslot` | Cox 1972 as `34(2), 187-220` | **187-202** — same discussion-span error |
| 7 | `bfsd` | "Verdinelli & Wasserman (1995); Dickey (1971)." — author-year only | Expanded both to full references (JASA 90(430) 614-618; Ann. Math. Statist. 42(1) 204-223) |
| 8 | `bnseff` | "Robins, Rotnitzky and Zhao (1994) for the influence function." — author-year only | Expanded (JASA 89(427) 846-866) |
| 9 | `ecod` | Year `2022` cited alongside `IEEE TKDE, 35(12), 12181-12193` — that is the **2023** issue | pending |
| 10 | `dpamp` | Balle et al. title truncated | Full title is "Privacy Amplification by Subsampling: Tight Analyses via Couplings and Divergences" — pending |

## Verified correct (checked individually, no change needed)

Athey & Imbens PNAS 113(27) 7353-7360 · Wager & Athey JASA 113(523)
1228-1242 · Kriegel KDD'08 444-452 · Ljung & Box Biometrika 65(2) 297-303
· Roberts/Gelman/Gilks AAP 7(1) · Sakurada & Yairi MLSDA 4-11 · Weibull
JAM 18 293-297 · Egozcue Math. Geol. 37(7) 795-828 · Astle & Balding
Stat. Sci. 24(4) · Hopcroft & Karp SIAM JC 2(4) 225-231 · Hahn
Econometrica 66 315-331 · Hirano/Imbens/Ridder Econometrica 71 1161-1189
· Breslow Biometrics 30(1) 89 · Carlstein Ann. Statist. 14(3) 1171-1179 ·
Rubin Biometrics 36(2) 293 · Bun & Steinke TCC 635-658 ·
Dwork/Rothblum/Vadhan FOCS 51-60 · Austin Stat. Med. 28(25) 3083-3107 ·
Cain & Lange Biometrics 40(2) 493 · Gronau JMP 81 80-97 · Verdinelli &
Wasserman JASA 90(430) 614-618 · Dickey Ann. Math. Statist. 42(1) 204-223
· Assimakopoulos & Nikolopoulos IJF 16(4) 521-530 · Hyndman & Billah IJF
19(2) 287-290 · Bennett Appl. Statist. 32(2) 165.

## Pattern worth naming

The two "discussion-span" errors (Cox & Snell, Cox 1972) are the same
mistake twice: read-with-discussion page ranges quoted as the article's
own. Both are JRSS-B papers where the discussion is bound with the
article. Worth checking any other JRSS-B citation in the corpus.

The four incomplete citations (`dpadam`, `fgsbh`, `bfsd`, `bnseff`) share
a different failure: enough to look like a citation, not enough to check
one. Those are the ones that hide errors, because nobody can falsify
them.

## Status

50 of 368 reviewed at first checkpoint. Audit continues.
