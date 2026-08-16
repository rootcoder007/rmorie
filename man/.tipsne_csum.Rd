% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.tipsne_csum}
\alias{.tipsne_csum}
\title{Neumaier-compensated sum. Written out rather than left to sum():}
\usage{
.tipsne_csum(v)
}
\arguments{
\item{v}{See Usage.}
}
\description{
CPython 3.12+ compensates a run of floats and R\\\'s sum() accumulates in long double, so the two built-ins are different functions and a comparison at the twelfth digit finds the difference.
}
\value{
A numeric value.
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
