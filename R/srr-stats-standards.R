# SPDX-License-Identifier: AGPL-3.0-or-later

#' srr_stats
#'
#' All of the following standards initially have `@srrstatsTODO` tags.
#' These may be moved at any time to any other locations in your code.
#' Once addressed, please modify the tag from `@srrstatsTODO` to `@srrstats`,
#' or `@srrstatsNA`, ensuring that references to every one of the following
#' standards remain somewhere within your code.
#' (These comments may be deleted at any time.)
#'
#' @srrstatsVerbose TRUE
#'
#' @noRd
NULL

#' NA_standards
#'
#' Any non-applicable standards can have their tags changed from `@srrstatsTODO`
#' to `@srrstatsNA`, and placed together in this block, along with explanations
#' for why each of these standards have been deemed not applicable.
#' (These comments may also be deleted at any time.)
#'
#'   factors; categorical predictors are treated as unordered, so the
#'   `ordered` factor expectations do not apply.
#'   non-standard class attributes; tabular inputs are plain numeric /
#'   character / factor columns.
#'   from simulated data and by cross-implementation parity (R vs the
#'   Python port, and wrappers vs their upstream CRAN packages), not by
#'   stored outputs from a published reference implementation.
#'   not part of the test suite; the tested property is statistical
#'   correctness, not computational scaling.
#'   environment variable gating a slower test tier); all tests run in
#'   the standard suite.
#'   is no separate extended-data test tier.
#'   is no download helper for reproducing them.
#'   as there is no extended-test tier.
#'   the methods/applications paper is in preparation and reports no
#'   comparative benchmarks, so there are no performance results to
#'   reproduce.
#'   alternative R packages, so no comparative-performance code applies.
#'   plain data.frames with documented coordinate / neighbourhood
#'   columns rather than requiring a dedicated spatial class as input;
#'   `sf` / `spdep` structures are constructed internally.
#'   there is no new-class conversion to document.
#'   spatial workflows are not provided; interoperability is via the
#'   wrapped spdep/sf/gstat calls themselves.
#'   loading GDAL/sf raster/vector files.
#'   PROJ6/WKT2 compliance is delegated entirely to `sf`.
#'   CRS metadata; CRS handling is left to `sf`.
#'   each method validates its own coordinate/neighbour input.
#'   carried, so there are none to preserve through pre-processing.
#'   density.
#'   models, so dimension-broadcasting concerns do not apply.
#'   the same class as the coordinate input.
#'   are returned.
#'   `plot` method; mapping is provided by the separate figure
#'   exporters (`morie_tps_figures`, Moran heatmap helpers).
#'   so coordinate-recovery tests do not apply.
#'   Cartesian-versus-curvilinear duality to test.
#'   tests do not apply to the projected-neighbourhood methods.
#'   knn / distance routines; rmorie does not re-test every neighbour
#'   definition exhaustively.
#'   re-tested here; see SP6.3)
#'   underlying dbscan / scan-statistic implementations.
#'   software, so its tests do not apply.
#'
#'   data; it does not implement extensive table filter/join operations
#'   requiring an index-column system.
#'   names; there is no join layer.
#'   `.morie_check_*` helpers; there is no attribute-bearing table layer
#'   that this standard targets.
#'   (e.g. automated variable/model selection reporting).
#'   input/model/output comparison layer.
#'   exposed; results are reported at full double precision and formatted
#'   for display only.
#'   package's own default plotting layer; rmorie's figures are produced
#'   by the separate exporters using ggplot2 defaults.
#'   from the underlying graphics packages.
#'   storage.mode annotations, as they operate on validated numeric data.
#'   by the figure exporters.
#'   is carried to annotate axes automatically.
#'   figure exporters are smoke-tested for successful file creation, not
#'   pixel-level appearance.
#'
#'   that need not carry row/column names, so non-default-name assertion
#'   does not apply.
#'   propagated onto the compact numeric result.
#'   where relevant, is discussed rather than silently applied.
#'   auto-transformation step is implemented; scaling is the caller's
#'   decision, documented per method.
#'   input; there is no missing-value handling parameter.
#'   (k-means / dbscan) native labels; they are not re-ordered by
#'   decreasing group size.
#'   provided; results are indexed positionally.
#'   computed for the submitted data set.
#'   generate-object-without-fitting mode.
#'   than printing large result matrices, so row-count restriction does
#'   not arise.
#'   `print` output is not implemented.
#'   visualisation is via the separate figure exporters.
#'   see UL6.0)
#'   placement; see UL6.0)
#'   violations yield invalid output are not part of the suite; input
#'   validation prevents the ill-posed cases instead.
#'   (UL3.0), there is no size-ordering to test.
#'   is no label recovery to test.
#'   relative efficiency cannot be tested.
#'
#'   section; prior specification is documented at the function level.
#'   guidance lives in the function documentation.
#'   values is not exposed; the Stan backends are run afresh.
#'   R-hat); there is no with/without-convergence-checker toggle.
#'   checkers cannot be compared.
#'   data.frame validation; its effects are not separately unit-tested
#'   for the Bayesian path.
#'   silent truncation of over-length distributional-parameter vectors.
#'   name rather than by positional vector length; see BS2.3)
#'   starting points.
#'   seeds; chain seeding is delegated to Stan.
#'   user-facing vector parameter, so the plural-naming rule does not
#'   apply.
#'   separately tested.
#'   implemented/tested for the Bayesian path.
#'   collinear data is implemented; such terms are dropped upstream.
#'   provided; the backends are the reference samplers (Stan).
#'   there is no stop-on-convergence mode.
#'   warnings rather than a bespoke mechanism.
#'   equivalence test is implemented (single fixed-iteration path).
#'   rmorie, so its parameters are not separately tested.
#'   no multiple-checker details to return.
#'   fit object rather than a separately returned rmorie summary.
#'   method; the returned Stan fit object can be plotted with the Stan
#'   ecosystem's own tools.
#'   not reimplemented here)
#'   object; not reimplemented here)
#'   part of the suite; correctness relies on the referenced Stan
#'   implementations.
#'   implemented.
#'   test is implemented.
#'   tested for the Bayesian path.
#'
#' @noRd
NULL
