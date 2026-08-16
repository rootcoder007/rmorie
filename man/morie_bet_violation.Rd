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
\item{theta}{See Usage.}
\item{level}{Defaults to \code{0.95}.}
\item{lower_bound}{Defaults to \code{0}.}
\item{draws}{Defaults to \code{20000L}.}
\item{seed}{Defaults to \code{0}.}
\item{grid}{Defaults to \code{NULL}.}
}
\description{
Part of the bndpcb_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{max_shortfall}, \code{at_cut}, \code{subset_coverage}, \code{subset_share}, \code{bet_proof}, \code{level}, \code{note}.
}
\keyword{internal}
