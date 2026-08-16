% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.tipsne_solve_chol}
\alias{.tipsne_solve_chol}
\title{Solve L L\\\' x = b by forward then back substitution}
\usage{
.tipsne_solve_chol(lo, b)
}
\arguments{
\item{lo}{See Usage.}
\item{b}{See Usage.}
}
\description{
Part of the tipsne_native implementation; see the file header for the source it follows.
}
\value{
The value of \code{x}, as built in the body.
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
