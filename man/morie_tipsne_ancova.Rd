% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_tipsne_ancova}
\alias{morie_tipsne_ancova}
\title{Least squares of y on a design matrix, via the normal equations}
\usage{
morie_tipsne_ancova(y, design)
}
\arguments{
\item{y}{Numeric outcome vector.}
\item{design}{Numeric matrix, one row per observation, including the   intercept column.}
}
\description{
Least squares of y on a design matrix, via the normal equations
}
\value{
A list with beta, the residual sum of squares, the residual   degrees of freedom, sigma2, the unscaled covariance and the fitted   values.
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
