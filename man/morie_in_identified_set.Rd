% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_in_identified_set}
\alias{morie_in_identified_set}
\title{morie_in_identified_set}
\usage{
morie_in_identified_set(freq, beta, gamma, x, alpha_grid, y0_values = c(0, 1), link = "logit", tol = 1e-04, iters = 4000L)
}
\arguments{
\item{freq}{See Usage.}
\item{beta}{See Usage.}
\item{gamma}{See Usage.}
\item{x}{See Usage.}
\item{alpha_grid}{See Usage.}
\item{y0_values}{Defaults to \code{c(0, 1)}.}
\item{link}{Defaults to \code{"logit"}.}
\item{tol}{Defaults to \code{1e-04}.}
\item{iters}{Defaults to \code{4000L}.}
}
\description{
Part of the bnshrt_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{discrepancy}, \code{feasible}, \code{weights}, \code{fitted}, \code{target}.
}
\keyword{internal}
