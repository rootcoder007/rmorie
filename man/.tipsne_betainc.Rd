% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.tipsne_betainc}
\alias{.tipsne_betainc}
\title{Regularised incomplete beta I_x(a, b) by the continued fraction}
\usage{
.tipsne_betainc(a, b, x)
}
\arguments{
\item{a}{See Usage.}
\item{b}{See Usage.}
\item{x}{See Usage.}
}
\description{
Part of the tipsne_native implementation; see the file header for the source it follows.
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
