# cran-comments.md — rmorie 1.2.1

NOT SUBMITTED TO CRAN. `.Rbuildignore`d; this is the local copy of the
text pasted into the comment box at https://cran.r-project.org/submit.html

## Test environments

* local (Fedora, R 4.6.1): `R CMD check --as-cran` — PENDING
* win-builder R-release and R-devel — uploaded 2026-09-08, results pending
* GitHub Actions: ubuntu release/devel/oldrel-1, windows, macOS, and a
  Debian bookworm container — all green on the submitted commit

## R CMD check results

PENDING — fill before submitting.

## On the size of the tarball

The tarball is 11.7 MB, above the 5 MB guideline. This is not bundled
data. The breakdown, compressed:

    R/          5.43 MB     2,844 source files
    man/        2.90 MB    11,418 documented topics
    inst/       2.18 MB
    tests/      0.89 MB
    src/        0.08 MB

`R/` alone exceeds 5 MB, so no amount of trimming data brings the package
under the guideline; only splitting it would, and the parts are not
independently useful -- the causal, spatial, survey-weighted, psychometric
and point-process modules share one estimator core and one dataset layer.

The two largest data files are load-bearing rather than convenience
payload: `describe_corpus.Rds` (1.7 MB) backs `morie_describe()` and its
tests, and `_unified_catalog.csv` (1.6 MB) is the dataset catalogue the
CLI resolves against. There are no vendored binaries, no compiled
third-party sources, and no bundled research corpora -- the datasets live
in the separate 'rmoriedata' package.

If the size is not acceptable we would rather hear it than have the
package rejected silently, and we are willing to split the package across
a release or two if that is the requirement.

## Concurrent submission of a dependency

rmorie Imports 'rmoriedata', which is being submitted in the same batch,
so the incoming check will report

    Strong dependencies not in the CRAN or BioC software repositories:
      rmoriedata

This is expected. 'rmoriedata' is a data-only package (no compiled code,
4.77 MB) and 'rmoriebricklayer', which both depend on, is already on CRAN.
We are happy for rmorie to be held until rmoriedata is published, and to
resubmit rather than have it processed out of order.

'Additional_repositories' resolves both non-CRAN sources; the check
reports availability "yes" for rmoriedata and cmdstanr.

## Reverse dependencies

None on CRAN.
