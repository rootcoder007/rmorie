% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_fairness_as_1d}
\alias{.morie_fairness_as_1d}
\title{.morie_fairness_as_1d}
\usage{
.morie_fairness_as_1d(x, name)
}
\arguments{
\item{x}{Coerced to vector by the body, with \code{as.vector}.}
\item{name}{Passed to \code{sprintf}.}
}
\description{
A step of the frns_metrics implementation. Called by \code{morie_fairness_average_odds_difference}, \code{morie_fairness_bias_amplification}, \code{morie_fairness_demographic_parity} and 3 others in the module. See the file header for the source the module follows.
}
\value{
The value of \code{arr}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.morie_fairness_as_1d(V, V)
}
