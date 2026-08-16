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
\examples{
N <- 60L
ii <- 0:(N - 1L)
ARM <- ifelse(ii\%\%2L == 0L, 1, 0)
COV <- cbind((ii\%\%5L) - 2, cos(0.7 * ii))
BASE <- 2 + 0.8 * COV[, 1] + 0.4 * COV[, 2] + 0.9 * ARM + 0.5 * 
    sin(3 * ii) + 0.3 * cos(7 * ii)
MISS <- as.integer((ARM == 1 & ii\%\%7L == 3L) | (ARM == 0 & ii\%\%11L == 
    5L))
DES <- cbind(1, ARM, COV[, 1], COV[, 2])[MISS == 0L, , drop = FALSE]
YO <- BASE[MISS == 0L]
morie_tipsne_ancova(YO, DES)
}
