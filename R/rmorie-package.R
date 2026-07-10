#' morie: Multi-Domain Open Research and Inferential Estimation
#'
#' Multi-domain scientific computing toolkit for observational
#' inference and intervention analysis across scientific-experimentation
#' contexts. Hosts the MRM (Multilevel Reconciliation Methodology)
#' framework as a primary application for Canadian carceral, police,
#' and oversight data.
#'
#' Provides general-purpose causal estimators (ATE, ATT, ATC, GATE,
#' CATE, LATE, AIPW, G-computation), survey sampling methods
#' (stratified, cluster, PPS, bootstrap, calibration weights),
#' propensity-score and doubly-robust estimators, and sensitivity
#' analyses (E-value, Rosenbaum bounds). Companion modules support
#' signal processing and spectral analysis, cryptographic primitives,
#' spatial statistics, statistical physics of crime (Hawkes self-
#' exciting processes, reaction-diffusion, Levy flight, urban
#' scaling), and classical-test-theory and item-response-theory
#' psychometrics, alongside ingestion utilities for officially
#' published Ontario Special Investigations Unit (SIU; police-
#' oversight) and federal Structured Intervention Unit reports.
#'
#' @section Statement of need:
#' Applied observational research on Canadian carceral, policing, and
#' oversight data has, until now, meant stitching together a dozen
#' single-purpose packages -- one for difference-in-differences, one
#' for propensity-score matching, one for spatial scan statistics, one
#' for self-exciting point processes -- each with its own data
#' contract, and none aware of the survey-weighting, provenance, and
#' privacy constraints these data carry. rmorie is aimed at
#' criminologists, quantitative social scientists, and
#' accountability researchers who need those estimators in one
#' consistent, provenance-preserving toolkit, with the MRM
#' (Multilevel Reconciliation Methodology) framework as its motivating
#' application. Every public function is prefixed \code{morie_*} to
#' compose safely alongside the specialist packages it wraps or
#' complements, and results carry the labeling required to keep
#' synthetic development runs from being mistaken for inferential
#' findings.
#'
#' @section Statistical methods and primary references:
#' The estimators implemented or wrapped here follow established
#' methodological literature. Primary references, by domain:
#' \itemize{
#'   \item \strong{Difference-in-differences} -- Callaway, B. and
#'     Sant'Anna, P. H. C. (2021). Difference-in-differences with
#'     multiple time periods. \emph{Journal of Econometrics},
#'     225(2), 200-230. de Chaisemartin, C. and D'Haultfoeuille, X.
#'     (2020). Two-way fixed effects estimators with heterogeneous
#'     treatment effects. \emph{American Economic Review}, 110(9),
#'     2964-2996.
#'   \item \strong{Propensity scores and doubly-robust estimation} --
#'     Rosenbaum, P. R. and Rubin, D. B. (1983). The central role of
#'     the propensity score in observational studies for causal
#'     effects. \emph{Biometrika}, 70(1), 41-55.
#'   \item \strong{Sensitivity analysis} -- VanderWeele, T. J. and
#'     Ding, P. (2017). Sensitivity analysis in observational
#'     research: introducing the E-value. \emph{Annals of Internal
#'     Medicine}, 167(4), 268-274.
#'   \item \strong{Self-exciting point processes} -- Hawkes, A. G.
#'     (1971). Spectra of some self-exciting and mutually exciting
#'     point processes. \emph{Biometrika}, 58(1), 83-90. Ogata, Y.
#'     (1988). Statistical models for earthquake occurrences and
#'     residual analysis for point processes. \emph{Journal of the
#'     American Statistical Association}, 83(401), 9-27.
#'   \item \strong{Spatial statistics} -- Moran, P. A. P. (1950).
#'     Notes on continuous stochastic phenomena. \emph{Biometrika},
#'     37(1/2), 17-23. Anselin, L. (1995). Local indicators of
#'     spatial association -- LISA. \emph{Geographical Analysis},
#'     27(2), 93-115. Kulldorff, M. (1997). A spatial scan statistic.
#'     \emph{Communications in Statistics -- Theory and Methods},
#'     26(6), 1481-1496.
#' }
#' Where an estimator is a thin wrapper over an established CRAN
#' package (the deliberate design principle of this package), the
#' wrapper documentation names the upstream implementation; rmorie's
#' own contribution is the unifying data contract, provenance
#' handling, and the MRM composition, not a reimplementation of the
#' underlying algorithm.
#'
#' @section Life cycle:
#' rmorie is \strong{experimental} (see the lifecycle badge in the
#' README). The \code{morie_*} public API is stabilising ahead of a
#' 1.x CRAN release; function signatures for the core causal, spatial,
#' and point-process estimators are considered stable, while newer
#' subsystems (SIU/OTIS ingestion, the AI helpers, and the
#' causal-taphonomy suite) may still change. Deprecations, when they
#' occur, will follow the standard warn-then-remove cycle across at
#' least one minor version.
#'
#' @section Acronyms used throughout the package:
#' \itemize{
#'   \item \strong{MRM} -- Multilevel Reconciliation Methodology
#'   \item \strong{OTIS} -- Offender Tracking Information System
#'     (published by the Ontario Ministry of the Solicitor General,
#'     formerly the Ministry of Community Safety and Correctional
#'     Services until June 2019)
#'   \item \strong{SIU} -- Special Investigations Unit
#'     (Ontario police-oversight body)
#'   \item \strong{TPS} -- Toronto Police Service
#'   \item \strong{CSI} -- Crime Severity Index (Statistics Canada)
#'   \item \strong{ac} -- alert complexity (per-person-year count of
#'     distinct alert-state configurations)
#'   \item \strong{vm} -- volatility measure of placements (per-
#'     person-year regional-transition count)
#' }
#'
#' @section Companion papers:
#' If you use rmorie in your work, please cite the relevant companion
#' papers (see \code{citation("rmorie")} for the current list). DOIs
#' are pending re-deposit.
#'
#' @section Key external citations used by MRM modules:
#' \itemize{
#'   \item Sprott, J. B. and Doob, A. N. (2021).
#'     \emph{Solitary Confinement, Torture, and Canada's Structured
#'     Intervention Units.}  Centre for Criminology and Sociolegal
#'     Studies, University of Toronto.
#'     Available at the Centre for Criminology and Sociolegal
#'     Studies web site: crimsl.utoronto.ca (file
#'     TortureSolitarySIUsSprottDoob23Feb2021_0.pdf).
#'   \item Doob, A. N. and Sprott, J. B. (2020).
#'     \emph{Understanding the Operation of Correctional Service
#'     Canada's Structured Intervention Units: Some Preliminary
#'     Findings.}  John Howard Society of Canada.
#'   \item Iftene, A. and Doob, A. N. (2024).
#'     \emph{Do Independent External Decision Makers Ensure that
#'     ``An Inmate's Confinement in a Structured Intervention Unit
#'     Is to End as Soon as Possible''? (Corrections and Conditional
#'     Release Act, Section 33).}  Dalhousie Schulich School of Law,
#'     report 51.
#'     \url{https://digitalcommons.schulichlaw.dal.ca/reports/51/}
#'   \item Structured Intervention Unit Implementation Advisory
#'     Panel (2024). \emph{Final Report on Structured Intervention
#'     Units and Solitary Confinement.}  Public Safety Canada.
#'   \item United Nations General Assembly (2015). \emph{United
#'     Nations Standard Minimum Rules for the Treatment of Prisoners
#'     (the Nelson Mandela Rules).}  Resolution A/RES/70/175.
#' }
#'
#' @section License:
#' morie is licensed under the \strong{GNU Affero General Public
#' License, version 3 or later} (\code{AGPL-3.0-or-later}), on both
#' the R and Python sides. The optional Linux-kernel adjuncts under
#' \code{kernel-module/} and \code{daemon/} are \code{GPL-2.0-only}
#' (kernel ABI requirement) and are not part of the CRAN tarball.
#'
#' @srrstats {G1.0} Primary methodological references for every
#'   estimator family are listed in the "Statistical methods and
#'   primary references" section above.
#' @srrstats {G1.1} rmorie is, by design, not a first implementation:
#'   its statistical estimators are thin wrappers over established
#'   CRAN packages (see the wrapper-as-extender principle); its
#'   contribution is the unifying data contract, provenance handling,
#'   and MRM composition. Wrapper docs name the upstream algorithm.
#' @srrstats {G1.2} A Life Cycle Statement is given in the "Life
#'   cycle" section above and mirrored by the README lifecycle badge.
#' @srrstats {G1.3} Statistical and domain terminology is defined in
#'   the "Acronyms used throughout the package" section and in each
#'   estimator's own documentation.
#' @srrstats {G1.4} All exported functions are documented with
#'   roxygen2; the entire man/ tree is roxygen-generated.
#' @keywords internal
#' @aliases rmorie-package
#' @importFrom stats aggregate anova ave deviance median na.omit plogis qf setNames update weighted.mean
#' @importFrom utils str
#' @importFrom Rcpp sourceCpp
#' @useDynLib rmorie, .registration = TRUE
"_PACKAGE"

# `weight` is referenced as a data-frame column name inside
# non-standard-evaluation contexts (e.g. `subset()`, `transform()`,
# `with()`); silence the R CMD check NOTE without weakening the rest
# of the codetools analysis.
utils::globalVariables(c("weight"))
