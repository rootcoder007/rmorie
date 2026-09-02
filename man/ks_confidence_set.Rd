% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{ks_confidence_set}
\alias{ks_confidence_set}
\title{ks_confidence_set}
\usage{
ks_confidence_set(moment_fn, theta_grid, X, form = "sum", n_equality = 0L, level = 0.95, 
    n_levels = 2L, reps = 100L, seed = 0L) 
}
\arguments{
\item{moment_fn}{Accepted by the signature and not used anywhere in the body.}
\item{theta_grid}{See Usage.}
\item{X}{Passed to \code{hypercube_instruments}.}
\item{form}{Carried through into a list the body builds. Defaults to \code{"sum"}.}
\item{n_equality}{Passed to \code{ks_statistic}. Defaults to \code{0L}.}
\item{level}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.95}.}
\item{n_levels}{Passed to \code{hypercube_instruments}. Defaults to \code{2L}.}
\item{reps}{Passed to \code{ks_critical_value}. Defaults to \code{100L}.}
\item{seed}{Passed to \code{ks_critical_value}. Defaults to \code{0L}.}
}
\description{
A step of the bnskmt_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{set}, \code{n_in_set}, \code{bounds}, \code{statistics}, \code{form}, \code{level}, \code{n_instruments}, \code{method}.
}
\keyword{internal}
\examples{
set.seed(3)
X <- matrix(runif(40), 20, 2)
mfn <- function(theta) X[, 1] - theta
r <- ks_confidence_set(mfn, theta_grid = seq(0.2, 0.8, by = 0.1), X)
str(r, max.level = 1)
}
