% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_tipsne_impute}
\alias{morie_tipsne_impute}
\title{One completed outcome vector under the chosen imputation route}
\usage{
morie_tipsne_impute(e, y, arm, X, miss, fit, mi)
}
\arguments{
\item{e}{A generator environment from .ghc_rng.}
\item{y}{Outcome vector, NA where missing.}
\item{arm}{Arm indicator.}
\item{X}{Covariates or NULL.}
\item{miss}{Integer missingness indicator.}
\item{fit}{The complete-case ANCOVA fit.}
\item{mi}{Imputation route.}
}
\description{
One completed outcome vector under the chosen imputation route
}
\value{
A completed numeric outcome vector.
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
