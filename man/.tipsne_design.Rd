% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.tipsne_design}
\alias{.tipsne_design}
\title{Intercept, arm indicator, then any covariates}
\usage{
.tipsne_design(arm, X, n)
}
\arguments{
\item{arm}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{X}{Optional; may be \code{NULL}. A matrix; passed to \code{as.matrix}.}
\item{n}{A count; the body uses it as \code{rep(...)}.}
}
\description{
A step of the tipsne_native implementation. Called by \code{morie_tipsne}, \code{morie_tipsne_impute}. See the file header for the source the module follows.
}
\value{
A matrix, from \code{matrix}.
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
\examples{
.tipsne_design(arm = c(1, 2, 3, 4, 5, 6, 7, 8), X = c(1, 2, 3, 4, 5, 6, 7, 8), n = 5L)
}
