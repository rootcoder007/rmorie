% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.tipsne_t_sf}
\alias{.tipsne_t_sf}
\title{Upper tail of Student\\'s t, from the regularised incomplete beta}
\usage{
.tipsne_t_sf(t, df)
}
\arguments{
\item{t}{Numeric; combined arithmetically in the body.}
\item{df}{Numeric; combined arithmetically in the body.}
}
\description{
Written out because pt() and the Python arm are separate implementations and would disagree in the last digits.
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
