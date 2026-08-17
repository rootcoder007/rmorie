% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_slice_sample_1d}
\alias{morie_slice_sample_1d}
\title{One univariate slice transition}
\usage{
morie_slice_sample_1d(logf, x0, e, w = 1, max_steps = 50L, lower = -Inf, upper = Inf)
}
\arguments{
\item{logf}{Function returning the log of an unnormalised density.}
\item{x0}{Current state.}
\item{e}{Generator environment (a \code{.ghc_rng} handle).}
\item{w}{Slice width; must be positive.}
\item{max_steps}{Maximum number of stepping-out steps per side.}
\item{lower,upper}{Support bounds; the draw stays in   \code{[lower, upper]} without any rejection.}
}
\description{
Returns the draw, the number of log-density evaluations, and the final slice interval. The chain stays in distribution regardless of the choice of \code{w}; what \code{w} affects is only the cost.
}
\value{
List with \code{x}, \code{n_eval}, \code{interval}.
}
\keyword{internal}
\examples{
e <- .ghc_rng(1)
logf <- function(x) -0.5 * x^2
morie_slice_sample_1d(logf, x0 = 0.5, e)
}
