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
#' @srrstatsTODO {G1.4a} *All internal (non-exported) functions should also be documented in standard [`roxygen2`](https://roxygen2.r-lib.org/) format, along with a final `@noRd` tag to suppress automatic generation of `.Rd` files or [`@keywords internal`](https://roxygen2.r-lib.org/reference/tags-index-crossref.html?q=keywords%20internal#null) if documentation is still desired.* 
#' @noRd
NULL

#' NA_standards
#'
#' Any non-applicable standards can have their tags changed from `@srrstatsTODO`
#' to `@srrstatsNA`, and placed together in this block, along with explanations
#' for why each of these standards have been deemed not applicable.
#' (These comments may also be deleted at any time.)
#'
#' @srrstatsNA {G2.5} No estimator distinguishes ordered from unordered
#'   factors; categorical predictors are treated as unordered, so the
#'   `ordered` factor expectations do not apply.
#' @srrstatsNA {G2.11} rmorie does not support `units`-style columns with
#'   non-standard class attributes; tabular inputs are plain numeric /
#'   character / factor columns.
#' @srrstatsNA {G5.4c} Correctness is established by parameter recovery
#'   from simulated data and by cross-implementation parity (R vs the
#'   Python port, and wrappers vs their upstream CRAN packages), not by
#'   stored outputs from a published reference implementation.
#' @srrstatsNA {G5.7} Algorithm performance / scaling with data size is
#'   not part of the test suite; the tested property is statistical
#'   correctness, not computational scaling.
#' @srrstatsNA {G5.10} There is no extended-test switch (e.g. an
#'   environment variable gating a slower test tier); all tests run in
#'   the standard suite.
#' @srrstatsNA {G5.11} No tests require large downloaded data, so there
#'   is no separate extended-data test tier.
#' @srrstatsNA {G5.11a} As no downloaded data is needed for tests, there
#'   is no download helper for reproducing them.
#' @srrstatsNA {G5.12} There are no extended-test conditions to document,
#'   as there is no extended-test tier.
#' @srrstatsNA {G1.5} No associated publication makes performance claims;
#'   the methods/applications paper is in preparation and reports no
#'   comparative benchmarks, so there are no performance results to
#'   reproduce.
#' @srrstatsNA {G1.6} rmorie makes no performance claims against
#'   alternative R packages, so no comparative-performance code applies.
#' @srrstatsNA {RE2.2} The estimators use complete-case selection; they
#'   do not offer separate missing-value handling for predictor versus
#'   response data.
#' @srrstatsNA {RE2.3} Centring / offsetting (z-scoring, zero-intercept)
#'   is left to the caller; no built-in centring parameter is provided.
#' @srrstatsNA {RE2.4b} Perfect collinearity between predictor and
#'   response is a degenerate case that surfaces as a fitting error
#'   rather than being separately pre-detected.
#' @srrstatsNA {RE4.1} Estimators always fit; there is no
#'   generate-object-without-fitting mode.
#' @srrstatsNA {RE4.7} The primary estimators are closed-form
#'   (OLS / IPW) with no iterative convergence; iterative backends report
#'   their own convergence diagnostics upstream.
#' @srrstatsNA {RE4.8} Response-variable metadata beyond the variable
#'   name is not attached to the compact result object.
#' @srrstatsNA {RE4.9} Per-observation modelled (fitted) response values
#'   are not returned by default; the result reports the estimate and its
#'   inference.
#' @srrstatsNA {RE4.13} Predictor-variable metadata is not attached to
#'   the result object.
#' @srrstatsNA {RE4.14} The DiD/causal estimators are not forecasting
#'   models; extrapolation / forecast errors do not apply (forecasting is
#'   handled by the time-series estimators).
#' @srrstatsNA {RE4.15} As above: no forecast errors to document for the
#'   regression estimators.
#' @srrstatsNA {RE4.16} The estimators do not model distinct responses
#'   per categorical group requiring submission of new group levels.
#' @srrstatsNA {RE4.18} A dedicated `summary` method beyond the rich
#'   `print` output is not implemented.
#' @srrstatsNA {RE5.0} Scaling of run time with data size is not tested;
#'   the tested property is statistical correctness.
#' @srrstatsNA {RE6.0} The compact result objects do not carry a default
#'   `plot` method; visualisation is provided by the separate figure
#'   exporters (`morie_otis_figures`, etc.).
#' @srrstatsNA {RE6.1} No default regression `plot` method, so its
#'   internals do not apply.
#' @srrstatsNA {RE6.2} As above, no default regression `plot` method.
#' @srrstatsNA {RE6.3} The regression estimators do not generate
#'   forecasts, so forecast-plotting does not apply.
#' @srrstatsNA {RE7.2} The compact result object does not retain input
#'   row / case names, so there is nothing to test for their retention.
#' @srrstatsNA {RE7.4} No forecast errors are produced by the regression
#'   estimators, so there are none to test.
#' @srrstatsNA {RE7.1a} Relative fitting speed on noiseless versus noisy
#'   data is a performance property that is not part of the test suite;
#'   correctness (exact recovery) is what is tested.
#'
#' @srrstatsNA {TS1.3} There is no single tsbox-style pre-processing
#'   routine; the heterogeneous point-process / volatility / survival
#'   models each validate their own domain-native input.
#' @srrstatsNA {TS1.4} No ts-class attributes are used, so there are none
#'   to preserve through pre-processing.
#' @srrstatsNA {TS1.7} `units`-package inputs are not supported (as with
#'   the general G2.11 position).
#' @srrstatsNA {TS2.0} The point-process models operate on irregular
#'   event times by construction; the regular-data / implicit-missing
#'   distinction does not apply.
#' @srrstatsNA {TS2.1} Missing-value handling options for regular series
#'   do not apply: the models require complete event-time / return series.
#' @srrstatsNA {TS2.1a} (see TS2.1)
#' @srrstatsNA {TS2.1b} (see TS2.1)
#' @srrstatsNA {TS2.1c} (see TS2.1)
#' @srrstatsNA {TS2.4} Stationarity is a documented model assumption; the
#'   software does not implement automatic stationarity checks or
#'   stationarity-inducing transforms.
#' @srrstatsNA {TS2.4a} (see TS2.4 — no automatic stationarity diagnostics)
#' @srrstatsNA {TS2.4b} (see TS2.4 — no automatic transforms)
#' @srrstatsNA {TS2.5} No auto-covariance matrix is returned, so there is
#'   no row/column ordering to tie to a time index.
#' @srrstatsNA {TS2.6} As above, no auto-covariance matrix and therefore
#'   no units to attach to one.
#' @srrstatsNA {TS3.0} The models estimate intensity / volatility /
#'   hazard; they do not produce multi-step-ahead forecasts, so
#'   forecast-error-versus-horizon behaviour does not apply.
#' @srrstatsNA {TS3.1} (no forecasting; see TS3.0)
#' @srrstatsNA {TS3.2} (no forecasting; see TS3.0)
#' @srrstatsNA {TS3.3} (no forecasting; see TS3.0)
#' @srrstatsNA {TS3.3a} (no forecasting; see TS3.0)
#' @srrstatsNA {TS3.3b} (no forecasting; see TS3.0)
#' @srrstatsNA {TS4.0a} No tsbox round-trip, as no ts-class is used.
#' @srrstatsNA {TS4.1} No units are carried on inputs, so none are
#'   returned.
#' @srrstatsNA {TS4.3} Return values carry no formal unit / time-scale
#'   attributes beyond the caller's own scale.
#' @srrstatsNA {TS4.4} No forecasting, so there are no transformation
#'   effects on forecasts to document.
#' @srrstatsNA {TS4.5} No forecasting / non-stationary back-transform path.
#' @srrstatsNA {TS4.5a} (see TS4.5)
#' @srrstatsNA {TS4.5b} (see TS4.5)
#' @srrstatsNA {TS4.5c} (see TS4.5)
#' @srrstatsNA {TS4.6} The models do not forecast, so no forecast
#'   distribution / moment object is returned.
#' @srrstatsNA {TS4.6a} (see TS4.6)
#' @srrstatsNA {TS4.6b} (see TS4.6)
#' @srrstatsNA {TS4.6c} (see TS4.6)
#' @srrstatsNA {TS4.7} No forecast values are produced, so there is
#'   nothing to distinguish from observed values.
#' @srrstatsNA {TS4.7a} (see TS4.7)
#' @srrstatsNA {TS4.7b} (see TS4.7)
#' @srrstatsNA {TS4.7c} (see TS4.7)
#' @srrstatsNA {TS5.0} The result objects carry no default temporal
#'   `plot` method; visualisation is via the separate figure exporters.
#' @srrstatsNA {TS5.1} (no default temporal plot method; see TS5.0)
#' @srrstatsNA {TS5.2} (see TS5.0)
#' @srrstatsNA {TS5.3} (see TS5.0)
#' @srrstatsNA {TS5.4} (see TS5.0)
#' @srrstatsNA {TS5.5} (see TS5.0)
#' @srrstatsNA {TS5.6} (see TS5.0)
#' @srrstatsNA {TS5.7} (see TS5.0)
#' @srrstatsNA {TS5.8} (see TS5.0)
#'
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
#' @srrstatsNA {ML1.0a} rmorie implements no ML training pipeline requiring labelled train/test/validation input data (assessment operates on the user's model).
#' @srrstatsNA {ML1.1} rmorie implements no ML training pipeline requiring labelled train/test/validation input data (assessment operates on the user's model).
#' @srrstatsNA {ML1.1a} rmorie implements no ML training pipeline requiring labelled train/test/validation input data (assessment operates on the user's model).
#' @srrstatsNA {ML1.1b} rmorie implements no ML training pipeline requiring labelled train/test/validation input data (assessment operates on the user's model).
#' @srrstatsNA {ML1.2} rmorie implements no ML training pipeline requiring labelled train/test/validation input data (assessment operates on the user's model).
#' @srrstatsNA {ML1.3} rmorie implements no ML training pipeline requiring labelled train/test/validation input data (assessment operates on the user's model).
#' @srrstatsNA {ML1.4} rmorie implements no ML training pipeline requiring labelled train/test/validation input data (assessment operates on the user's model).
#' @srrstatsNA {ML1.5} rmorie implements no ML training pipeline requiring labelled train/test/validation input data (assessment operates on the user's model).
#' @srrstatsNA {ML1.6} rmorie implements no ML training pipeline requiring labelled train/test/validation input data (assessment operates on the user's model).
#' @srrstatsNA {ML1.6a} rmorie implements no ML training pipeline requiring labelled train/test/validation input data (assessment operates on the user's model).
#' @srrstatsNA {ML1.6b} rmorie implements no ML training pipeline requiring labelled train/test/validation input data (assessment operates on the user's model).
#' @srrstatsNA {ML1.7} rmorie implements no ML training pipeline requiring labelled train/test/validation input data (assessment operates on the user's model).
#' @srrstatsNA {ML1.7a} rmorie implements no ML training pipeline requiring labelled train/test/validation input data (assessment operates on the user's model).
#' @srrstatsNA {ML1.7b} rmorie implements no ML training pipeline requiring labelled train/test/validation input data (assessment operates on the user's model).
#' @srrstatsNA {ML1.8} rmorie implements no ML training pipeline requiring labelled train/test/validation input data (assessment operates on the user's model).
#' @srrstatsNA {ML2.0} rmorie has no dedicated pre-processing-specification stage returning classed objects.
#' @srrstatsNA {ML2.0a} rmorie has no dedicated pre-processing-specification stage returning classed objects.
#' @srrstatsNA {ML2.0b} rmorie has no dedicated pre-processing-specification stage returning classed objects.
#' @srrstatsNA {ML2.1} rmorie has no dedicated pre-processing-specification stage returning classed objects.
#' @srrstatsNA {ML2.2} rmorie has no dedicated pre-processing-specification stage returning classed objects.
#' @srrstatsNA {ML2.2a} rmorie has no dedicated pre-processing-specification stage returning classed objects.
#' @srrstatsNA {ML2.2b} rmorie has no dedicated pre-processing-specification stage returning classed objects.
#' @srrstatsNA {ML2.3} rmorie has no dedicated pre-processing-specification stage returning classed objects.
#' @srrstatsNA {ML2.4} rmorie has no dedicated pre-processing-specification stage returning classed objects.
#' @srrstatsNA {ML2.5} rmorie has no dedicated pre-processing-specification stage returning classed objects.
#' @srrstatsNA {ML2.6} rmorie has no dedicated pre-processing-specification stage returning classed objects.
#' @srrstatsNA {ML2.7} rmorie has no dedicated pre-processing-specification stage returning classed objects.
#' @srrstatsNA {ML3.0} rmorie has no model-specification stage, training rates, optimizers, or epochs.
#' @srrstatsNA {ML3.0a} rmorie has no model-specification stage, training rates, optimizers, or epochs.
#' @srrstatsNA {ML3.0b} rmorie has no model-specification stage, training rates, optimizers, or epochs.
#' @srrstatsNA {ML3.0c} rmorie has no model-specification stage, training rates, optimizers, or epochs.
#' @srrstatsNA {ML3.0d} rmorie has no model-specification stage, training rates, optimizers, or epochs.
#' @srrstatsNA {ML3.1} rmorie has no model-specification stage, training rates, optimizers, or epochs.
#' @srrstatsNA {ML3.2} rmorie has no model-specification stage, training rates, optimizers, or epochs.
#' @srrstatsNA {ML3.3} rmorie has no model-specification stage, training rates, optimizers, or epochs.
#' @srrstatsNA {ML3.4} rmorie has no model-specification stage, training rates, optimizers, or epochs.
#' @srrstatsNA {ML3.4a} rmorie has no model-specification stage, training rates, optimizers, or epochs.
#' @srrstatsNA {ML3.4b} rmorie has no model-specification stage, training rates, optimizers, or epochs.
#' @srrstatsNA {ML3.5} rmorie has no model-specification stage, training rates, optimizers, or epochs.
#' @srrstatsNA {ML3.5a} rmorie has no model-specification stage, training rates, optimizers, or epochs.
#' @srrstatsNA {ML3.5b} rmorie has no model-specification stage, training rates, optimizers, or epochs.
#' @srrstatsNA {ML3.6} rmorie has no model-specification stage, training rates, optimizers, or epochs.
#' @srrstatsNA {ML3.6a} rmorie has no model-specification stage, training rates, optimizers, or epochs.
#' @srrstatsNA {ML3.6b} rmorie has no model-specification stage, training rates, optimizers, or epochs.
#' @srrstatsNA {ML3.7} rmorie has no model-specification stage, training rates, optimizers, or epochs.
#' @srrstatsNA {ML4.0} rmorie has no training loop with loss-function paths, gradients, batch processing, or resampling scheduler.
#' @srrstatsNA {ML4.1} rmorie has no training loop with loss-function paths, gradients, batch processing, or resampling scheduler.
#' @srrstatsNA {ML4.1a} rmorie has no training loop with loss-function paths, gradients, batch processing, or resampling scheduler.
#' @srrstatsNA {ML4.1b} rmorie has no training loop with loss-function paths, gradients, batch processing, or resampling scheduler.
#' @srrstatsNA {ML4.1c} rmorie has no training loop with loss-function paths, gradients, batch processing, or resampling scheduler.
#' @srrstatsNA {ML4.2} rmorie has no training loop with loss-function paths, gradients, batch processing, or resampling scheduler.
#' @srrstatsNA {ML4.3} rmorie has no training loop with loss-function paths, gradients, batch processing, or resampling scheduler.
#' @srrstatsNA {ML4.4} rmorie has no training loop with loss-function paths, gradients, batch processing, or resampling scheduler.
#' @srrstatsNA {ML4.5} rmorie has no training loop with loss-function paths, gradients, batch processing, or resampling scheduler.
#' @srrstatsNA {ML4.6} rmorie has no training loop with loss-function paths, gradients, batch processing, or resampling scheduler.
#' @srrstatsNA {ML4.7} rmorie has no training loop with loss-function paths, gradients, batch processing, or resampling scheduler.
#' @srrstatsNA {ML4.8} rmorie has no training loop with loss-function paths, gradients, batch processing, or resampling scheduler.
#' @srrstatsNA {ML4.8a} rmorie has no training loop with loss-function paths, gradients, batch processing, or resampling scheduler.
#' @srrstatsNA {ML5.0} rmorie produces no trained-model object or serialization of its own; assessment wraps the user's fitted model via fit_fn/predict_fn.
#' @srrstatsNA {ML5.0a} rmorie produces no trained-model object or serialization of its own; assessment wraps the user's fitted model via fit_fn/predict_fn.
#' @srrstatsNA {ML5.0b} rmorie produces no trained-model object or serialization of its own; assessment wraps the user's fitted model via fit_fn/predict_fn.
#' @srrstatsNA {ML5.1} rmorie produces no trained-model object or serialization of its own; assessment wraps the user's fitted model via fit_fn/predict_fn.
#' @srrstatsNA {ML5.2} rmorie produces no trained-model object or serialization of its own; assessment wraps the user's fitted model via fit_fn/predict_fn.
#' @srrstatsNA {ML5.2a} rmorie produces no trained-model object or serialization of its own; assessment wraps the user's fitted model via fit_fn/predict_fn.
#' @srrstatsNA {ML5.2b} rmorie produces no trained-model object or serialization of its own; assessment wraps the user's fitted model via fit_fn/predict_fn.
#' @srrstatsNA {ML5.2c} rmorie produces no trained-model object or serialization of its own; assessment wraps the user's fitted model via fit_fn/predict_fn.
#' @srrstatsNA {ML5.4b} rmorie produces no trained-model object or serialization of its own; assessment wraps the user's fitted model via fit_fn/predict_fn.
#' @srrstatsNA {ML7.0} there is no training pipeline (optimizers/loss/epochs/training-rates/trained-model classes) to test.
#' @srrstatsNA {ML7.1} there is no training pipeline (optimizers/loss/epochs/training-rates/trained-model classes) to test.
#' @srrstatsNA {ML7.2} there is no training pipeline (optimizers/loss/epochs/training-rates/trained-model classes) to test.
#' @srrstatsNA {ML7.3} there is no training pipeline (optimizers/loss/epochs/training-rates/trained-model classes) to test.
#' @srrstatsNA {ML7.3a} there is no training pipeline (optimizers/loss/epochs/training-rates/trained-model classes) to test.
#' @srrstatsNA {ML7.3b} there is no training pipeline (optimizers/loss/epochs/training-rates/trained-model classes) to test.
#' @srrstatsNA {ML7.4} there is no training pipeline (optimizers/loss/epochs/training-rates/trained-model classes) to test.
#' @srrstatsNA {ML7.5} there is no training pipeline (optimizers/loss/epochs/training-rates/trained-model classes) to test.
#' @srrstatsNA {ML7.6} there is no training pipeline (optimizers/loss/epochs/training-rates/trained-model classes) to test.
#' @srrstatsNA {ML7.7} there is no training pipeline (optimizers/loss/epochs/training-rates/trained-model classes) to test.
#' @srrstatsNA {ML7.8} there is no training pipeline (optimizers/loss/epochs/training-rates/trained-model classes) to test.
#' @srrstatsNA {ML7.9} there is no training pipeline (optimizers/loss/epochs/training-rates/trained-model classes) to test.
#' @srrstatsNA {ML7.9a} there is no training pipeline (optimizers/loss/epochs/training-rates/trained-model classes) to test.
#' @srrstatsNA {ML7.10} there is no training pipeline (optimizers/loss/epochs/training-rates/trained-model classes) to test.
#' @srrstatsNA {ML7.11} there is no training pipeline (optimizers/loss/epochs/training-rates/trained-model classes) to test.
#' @srrstatsNA {ML7.11a} there is no training pipeline (optimizers/loss/epochs/training-rates/trained-model classes) to test.
#' @noRd
NULL
