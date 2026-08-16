% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{linear_fluctuation_unsafe}
\alias{linear_fluctuation_unsafe}
\title{The linear update, kept so its failure is demonstrable}
\usage{
linear_fluctuation_unsafe(Q, H, Y)
}
\arguments{
\item{Q}{Initial outcome regression.}
\item{H}{Clever covariate.}
\item{Y}{Outcome.}
}
\description{
Squared-error loss with an additive fluctuation is unbounded: the updated fit can leave the outcome\'s range, and then so can the substitution estimator.
}
\value{
A list with \code{epsilon}, \code{Q_star},   \code{out_of_range}, \code{caveat}.
}
\keyword{internal}
