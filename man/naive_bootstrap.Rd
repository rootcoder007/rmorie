% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{naive_bootstrap}
\alias{naive_bootstrap}
\title{Compat with the Python arm: expose the three entry points}
\usage{
naive_bootstrap(data, estimator, B = 200L, seed = 0L) 
}
\arguments{
\item{data}{Passed to \code{morie_tlboot}.}
\item{estimator}{Passed to \code{morie_tlboot}.}
\item{B}{Passed to \code{morie_tlboot}. Defaults to \code{200L}.}
\item{seed}{Passed to \code{morie_tlboot}. Defaults to \code{0L}.}
}
\description{
A step of the tlboot_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{morie_tlboot}.
}
\keyword{internal}
\examples{
if (requireNamespace("ranger", quietly = TRUE)) {
  set.seed(1)
  d <- rnorm(50, 1)
  r <- naive_bootstrap(d, estimator = function(s) mean(unlist(s)), B = 200L)
  str(r, max.level = 1)
}
}
