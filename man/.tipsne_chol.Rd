% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.tipsne_chol}
\alias{.tipsne_chol}
\title{Cholesky factor L with A = L L\\', lower triangular. Explicit rather}
\usage{
.tipsne_chol(a)
}
\arguments{
\item{a}{A matrix; indexed by row and column.}
}
\description{
than chol() so the Python arm can match it element by element.
}
\value{
The value of \code{lo}, as built in the body.
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
