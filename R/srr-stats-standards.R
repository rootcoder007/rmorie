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
#' @srrstatsNA {SP2.0} For accessibility the spatial functions accept
#'   plain data.frames with documented coordinate / neighbourhood
#'   columns rather than requiring a dedicated spatial class as input;
#'   `sf` / `spdep` structures are constructed internally.
#' @srrstatsNA {SP2.0a} rmorie implements no new spatial input class, so
#'   there is no new-class conversion to document.
#' @srrstatsNA {SP2.2b} Round-trip translation tests into external
#'   spatial workflows are not provided; interoperability is via the
#'   wrapped spdep/sf/gstat calls themselves.
#' @srrstatsNA {SP2.3} Tests use bundled coordinate data rather than
#'   loading GDAL/sf raster/vector files.
#' @srrstatsNA {SP2.4} rmorie performs no coordinate reprojection, so
#'   PROJ6/WKT2 compliance is delegated entirely to `sf`.
#' @srrstatsNA {SP2.4a} (see SP2.4 — no CRS strings handled by rmorie)
#' @srrstatsNA {SP2.5} rmorie implements no spatial input class carrying
#'   CRS metadata; CRS handling is left to `sf`.
#' @srrstatsNA {SP2.5a} (see SP2.5 — no new spatial classes)
#' @srrstatsNA {SP2.8} There is no single spatial pre-processing routine;
#'   each method validates its own coordinate/neighbour input.
#' @srrstatsNA {SP2.9} No dedicated spatial metadata attributes are
#'   carried, so there are none to preserve through pre-processing.
#' @srrstatsNA {SP3.2} No routine samples input data by local spatial
#'   density.
#' @srrstatsNA {SP3.5} rmorie implements no spatial machine-learning
#'   models, so dimension-broadcasting concerns do not apply.
#' @srrstatsNA {SP3.6} (no spatial ML; sampling-procedure effects N/A)
#' @srrstatsNA {SP4.0a} Results are returned as analysis objects, not in
#'   the same class as the coordinate input.
#' @srrstatsNA {SP4.1} No units are carried on coordinate inputs, so none
#'   are returned.
#' @srrstatsNA {SP5.0} The spatial result objects carry no default
#'   `plot` method; mapping is provided by the separate figure
#'   exporters (`morie_tps_figures`, Moran heatmap helpers).
#' @srrstatsNA {SP5.1} (no default spatial plot method; see SP5.0)
#' @srrstatsNA {SP5.2} (see SP5.0)
#' @srrstatsNA {SP5.3} Interactive html visualisation is not implemented.
#' @srrstatsNA {SP6.0} rmorie implements no coordinate-transform routine,
#'   so coordinate-recovery tests do not apply.
#' @srrstatsNA {SP6.1} The methods are planar-only; there is no
#'   Cartesian-versus-curvilinear duality to test.
#' @srrstatsNA {SP6.1a} (planar-only; see SP6.1)
#' @srrstatsNA {SP6.1b} (planar-only; see SP6.1)
#' @srrstatsNA {SP6.2} Extreme-geographical-coordinate (polar/dateline)
#'   tests do not apply to the projected-neighbourhood methods.
#' @srrstatsNA {SP6.3} Neighbour construction delegates to spdep's tested
#'   knn / distance routines; rmorie does not re-test every neighbour
#'   definition exhaustively.
#' @srrstatsNA {SP6.4} (weighting schemes are spdep's; not exhaustively
#'   re-tested here; see SP6.3)
#' @srrstatsNA {SP6.5} Spatial clustering correctness is delegated to the
#'   underlying dbscan / scan-statistic implementations.
#' @srrstatsNA {SP6.6} rmorie implements no spatial machine-learning
#'   software, so its tests do not apply.
#'
#' @srrstatsNA {EA2.0} rmorie's exploratory layer summarises and tests
#'   data; it does not implement extensive table filter/join operations
#'   requiring an index-column system.
#' @srrstatsNA {EA2.1} (no index-column system; see EA2.0)
#' @srrstatsNA {EA2.2} (see EA2.0)
#' @srrstatsNA {EA2.2a} (see EA2.0)
#' @srrstatsNA {EA2.2b} (see EA2.0)
#' @srrstatsNA {EA2.3} No table joins are performed on assumed column
#'   names; there is no join layer.
#' @srrstatsNA {EA2.4} No multi-table `DM`-style class system is used.
#' @srrstatsNA {EA2.5} (no index-column tables; see EA2.0)
#' @srrstatsNA {EA2.6} Vector inputs are validated by the shared
#'   `.morie_check_*` helpers; there is no attribute-bearing table layer
#'   that this standard targets.
#' @srrstatsNA {EA3.0} rmorie does not automate meta-level extraction
#'   (e.g. automated variable/model selection reporting).
#' @srrstatsNA {EA3.1} rmorie provides no standardised cross-tool
#'   input/model/output comparison layer.
#' @srrstatsNA {EA4.1} No explicit numeric-precision control parameter is
#'   exposed; results are reported at full double precision and formatted
#'   for display only.
#' @srrstatsNA {EA5.0} Graphical accessibility standards target the
#'   package's own default plotting layer; rmorie's figures are produced
#'   by the separate exporters using ggplot2 defaults.
#' @srrstatsNA {EA5.0a} (see EA5.0)
#' @srrstatsNA {EA5.0b} (see EA5.0)
#' @srrstatsNA {EA5.1} rmorie does not override typeface specifications
#'   from the underlying graphics packages.
#' @srrstatsNA {EA5.3} Column summaries report values, not per-column
#'   storage.mode annotations, as they operate on validated numeric data.
#' @srrstatsNA {EA5.4} Visual rounding is left to the ggplot2 scales used
#'   by the figure exporters.
#' @srrstatsNA {EA5.5} Axis units are the caller's own; no unit metadata
#'   is carried to annotate axes automatically.
#' @srrstatsNA {EA5.6} rmorie bundles no dynamic-visualisation library.
#' @srrstatsNA {EA6.1} Graphical output is not tested via `vdiffr`; the
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
