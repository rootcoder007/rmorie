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
#' @srrstatsNA {PD2.0} Distributions are fitted and summarised directly
#'   rather than represented through a general distribution-object
#'   package (e.g. `distributional`).
#' @srrstatsNA {PD3.0} The heavy-tailed fits are numeric (Hill MLE); no
#'   analytic distribution algebra is performed.
#' @srrstatsNA {PD3.1} Operations are specific to the fitted models and
#'   are not organised as generic functions taking a distribution name.
#' @srrstatsNA {PD3.4} rmorie implements no probability-distribution
#'   integration routines.
#' @srrstatsNA {PD3.5} No integral is approximated by discrete summation.
#' @srrstatsNA {PD3.5a} (no discrete-summation integration; see PD3.5)
#'
#' @srrstatsNA {UL1.2} The methods operate on numeric coordinate matrices
#'   that need not carry row/column names, so non-default-name assertion
#'   does not apply.
#' @srrstatsNA {UL1.3} Row/column names are not required and are not
#'   propagated onto the compact numeric result.
#' @srrstatsNA {UL1.4b} Examples do not use `scale()` implicitly; scaling,
#'   where relevant, is discussed rather than silently applied.
#' @srrstatsNA {UL2.0} No automatic assumption-violation diagnostics or
#'   auto-transformation step is implemented; scaling is the caller's
#'   decision, documented per method.
#' @srrstatsNA {UL2.2} The unsupervised methods require complete numeric
#'   input; there is no missing-value handling parameter.
#' @srrstatsNA {UL3.0} Cluster labels are the underlying algorithm's
#'   (k-means / dbscan) native labels; they are not re-ordered by
#'   decreasing group size.
#' @srrstatsNA {UL3.2} Case labelling of unlabelled array input is not
#'   provided; results are indexed positionally.
#' @srrstatsNA {UL3.3} There is no predict-on-new-data method; results are
#'   computed for the submitted data set.
#' @srrstatsNA {UL4.1} The methods always fit; there is no
#'   generate-object-without-fitting mode.
#' @srrstatsNA {UL4.3a} The `print` method summarises parameters rather
#'   than printing large result matrices, so row-count restriction does
#'   not arise.
#' @srrstatsNA {UL4.4} A dedicated `summary` method beyond the rich
#'   `print` output is not implemented.
#' @srrstatsNA {UL6.0} The result objects carry no default `plot` method;
#'   visualisation is via the separate figure exporters.
#' @srrstatsNA {UL6.1} (no default clustering/ordination plot dispatch;
#'   see UL6.0)
#' @srrstatsNA {UL6.2} (no default plot method, so no automatic label
#'   placement; see UL6.0)
#' @srrstatsNA {UL7.1} Deliberate demonstrations that assumption
#'   violations yield invalid output are not part of the suite; input
#'   validation prevents the ill-posed cases instead.
#' @srrstatsNA {UL7.2} As cluster labels are not re-ordered by size
#'   (UL3.0), there is no size-ordering to test.
#' @srrstatsNA {UL7.3} Input labels are not propagated (UL1.3), so there
#'   is no label recovery to test.
#' @srrstatsNA {UL7.4} There is no new-data prediction method, so its
#'   relative efficiency cannot be tested.
#' @srrstatsNA {UL7.5} No batch-processing routines are implemented.
#' @srrstatsNA {UL7.5a} (no batch processing; see UL7.5)
#'
#' @srrstatsNA {BS1.2a} The README does not carry a Bayesian prior-example
#'   section; prior specification is documented at the function level.
#' @srrstatsNA {BS1.2b} There is no dedicated Bayesian vignette; prior
#'   guidance lives in the function documentation.
#' @srrstatsNA {BS1.3a} Using the output of a previous run as starting
#'   values is not exposed; the Stan backends are run afresh.
#' @srrstatsNA {BS1.4} There is a single convergence diagnostic (Stan
#'   R-hat); there is no with/without-convergence-checker toggle.
#' @srrstatsNA {BS1.5} Only one convergence checker is used, so multiple
#'   checkers cannot be compared.
#' @srrstatsNA {BS2.1a} The dimensional pre-processing is the standard
#'   data.frame validation; its effects are not separately unit-tested
#'   for the Bayesian path.
#' @srrstatsNA {BS2.3} Prior vectors are consumed by name; there is no
#'   silent truncation of over-length distributional-parameter vectors.
#' @srrstatsNA {BS2.4} (prior parameters are matched to model terms by
#'   name rather than by positional vector length; see BS2.3)
#' @srrstatsNA {BS2.8} Results of previous runs cannot be supplied as
#'   starting points.
#' @srrstatsNA {BS2.10} No diagnostic is issued for identical per-chain
#'   seeds; chain seeding is delegated to Stan.
#' @srrstatsNA {BS2.11} Starting values are not supplied as a
#'   user-facing vector parameter, so the plural-naming rule does not
#'   apply.
#' @srrstatsNA {BS2.14} Warning suppression for the Bayesian path is not
#'   separately tested.
#' @srrstatsNA {BS2.15} Error-to-warning conversion is not separately
#'   implemented/tested for the Bayesian path.
#' @srrstatsNA {BS3.2} No distinct sampling-bypass routine for perfectly
#'   collinear data is implemented; such terms are dropped upstream.
#' @srrstatsNA {BS4.1} No explicit comparison against external samplers is
#'   provided; the backends are the reference samplers (Stan).
#' @srrstatsNA {BS4.4} Sampling runs for a fixed number of iterations;
#'   there is no stop-on-convergence mode.
#' @srrstatsNA {BS4.5} Non-convergence is surfaced through Stan's R-hat
#'   warnings rather than a bespoke mechanism.
#' @srrstatsNA {BS4.6} No convergence-checker-versus-fixed-samples
#'   equivalence test is implemented (single fixed-iteration path).
#' @srrstatsNA {BS4.7} The convergence checker is not parametrised by
#'   rmorie, so its parameters are not separately tested.
#' @srrstatsNA {BS5.4} Only one convergence checker is used, so there are
#'   no multiple-checker details to return.
#' @srrstatsNA {BS5.5} Non-convergence diagnostics are those of the Stan
#'   fit object rather than a separately returned rmorie summary.
#' @srrstatsNA {BS6.0} rmorie implements no default posterior `plot`
#'   method; the returned Stan fit object can be plotted with the Stan
#'   ecosystem's own tools.
#' @srrstatsNA {BS6.1} (no default posterior plot method; see BS6.0)
#' @srrstatsNA {BS6.2} (posterior-sequence plotting via the Stan object;
#'   not reimplemented here)
#' @srrstatsNA {BS6.3} (posterior-distribution plotting via the Stan
#'   object; not reimplemented here)
#' @srrstatsNA {BS6.4} (optional posterior plots not implemented)
#' @srrstatsNA {BS6.5} (combined sample/estimate plots not implemented)
#' @srrstatsNA {BS7.0} A dedicated prior-parameter-recovery test is not
#'   part of the suite; correctness relies on the referenced Stan
#'   implementations.
#' @srrstatsNA {BS7.1} No prior-in-absence-of-data recovery test is
#'   implemented.
#' @srrstatsNA {BS7.2} No specified-prior-to-expected-posterior recovery
#'   test is implemented.
#' @srrstatsNA {BS7.3} Algorithmic-efficiency scaling is not tested.
#' @srrstatsNA {BS7.4} Fitted-value scale-equivalence is not separately
#'   tested for the Bayesian path.
#' @srrstatsNA {BS7.4a} (no scale-assumption test; see BS7.4)
#'
#' @noRd
NULL
