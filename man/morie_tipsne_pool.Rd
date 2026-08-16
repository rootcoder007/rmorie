% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_tipsne_pool}
\alias{morie_tipsne_pool}
\title{Combine per-imputation estimates and variances by Rubin's rules}
\usage{
morie_tipsne_pool(ests, vars, pooling = "rubin1987", df_complete = NULL)
}
\arguments{
\item{ests}{Point estimates, one per imputation.}
\item{vars}{Squared standard errors, one per imputation.}
\item{pooling}{"rubin1987" or "barnard_rubin".}
\item{df_complete}{Complete-data residual degrees of freedom;   required by the Barnard-Rubin route.}
}
\description{
Combine per-imputation estimates and variances by Rubin's rules
}
\value{
A list with the pooled estimate, standard error, degrees of   freedom, t statistic, p-value and the variance decomposition.
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
