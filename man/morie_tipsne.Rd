% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_tipsne}
\alias{morie_tipsne}
\title{Delta-adjusted tipping-point sensitivity analysis}
\usage{
morie_tipsne(y, D, missing_indicator = NULL, X = NULL, delta_treat = NULL, delta_control = NULL, n_imputations = 20L, seed = 1, alpha = 0.05, mi = "proper", pooling = "rubin1987", standardise = TRUE)
}
\arguments{
\item{y}{Outcome; NA where missing.}
\item{D}{Arm indicator, 0 control and 1 treatment.}
\item{missing_indicator}{1 where the outcome is missing. Derived from   y when omitted; a disagreement between the two raises rather than   silently picking one.}
\item{X}{Covariates for the ANCOVA, complete for every unit.}
\item{delta_treat}{Shifts applied to imputed values in the treated   arm. The default sweeps 0 down to -2.5 pooled standard deviations in   eleven steps.}
\item{delta_control}{Shifts applied in the control arm; 0 by default,   which is the one-way analysis. Pass both for a two-way grid.}
\item{n_imputations}{m. Ignored by the deterministic route.}
\item{seed}{Seed for the shared generator. Every grid cell is imputed   from the SAME seed, so a difference between cells is the delta and   not the draws.}
\item{alpha}{Significance level the crossing is measured against.}
\item{mi}{"proper", "improper" or "deterministic".}
\item{pooling}{"rubin1987" or "barnard_rubin".}
\item{standardise}{Report the deltas in pooled standard deviations as   well as the outcome's own units.}
}
\description{
Delta-adjusted tipping-point sensitivity analysis
}
\value{
A list with the MAR analysis, the grid, the tipping point for   each control-arm delta, and whether the result tipped at all.
}
\references{
  Rubin, D.B. (1987) "Multiple Imputation for Nonresponse in Surveys."
    Wiley. Chapter 3: the combining rules.
  Barnard, J. and Rubin, D.B. (1999) "Small-sample degrees of freedom
    with multiple imputation." Biometrika 86(4), 948-955.
  Yan, X., Lee, S., Ling, N. and Lin, J. (2021) tipping-point
    sensitivity analysis for MNAR departures in clinical trials; the
    delta-adjustment procedure here follows the now-standard regulatory
    form (impute under MAR, shift imputed values by delta per arm,
    re-analyse, locate the crossing).
}
\keyword{internal}
