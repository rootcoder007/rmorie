% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_bet_violation}
\alias{morie_bet_violation}
\title{morie_bet_violation}
\usage{
morie_bet_violation(theta, level = 0.95, lower_bound = 0, draws = 20000L, seed = 0, grid = NULL)
}
\arguments{
\item{theta}{Passed to \code{morie_coverage_by_region}.}
\item{level}{Passed to \code{morie_coverage_by_region}. Defaults to \code{0.95}.}
\item{lower_bound}{Passed to \code{morie_coverage_by_region}. Defaults to \code{0}.}
\item{draws}{Passed to \code{morie_coverage_by_region}. Defaults to \code{20000L}.}
\item{seed}{Passed to \code{morie_coverage_by_region}. Defaults to \code{0}.}
\item{grid}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the bndpcb_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{max_shortfall}, \code{at_cut}, \code{subset_coverage}, \code{subset_share}, \code{bet_proof}, \code{level}, \code{note}.
}
\keyword{internal}
