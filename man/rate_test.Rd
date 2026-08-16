% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{rate_test}
\alias{rate_test}
\title{rate_test}
\usage{
rate_test(scores, priority, weight = "autoc", reps = 500, seed = 0)
}
\arguments{
\item{scores}{Passed to \code{.slvgrf_check}.}
\item{priority}{Passed to \code{.slvgrf_check}.}
\item{weight}{Defaults to \code{"autoc"}.}
\item{reps}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{500}.}
\item{seed}{Passed to \code{.ghc_rng}. Defaults to \code{0}.}
}
\description{
A step of the slvgrf_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{se}, \code{z}, \code{p_value}, \code{weight}, \code{reps}, \code{n}, \code{null}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
rate_test(V, V)
}
