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
\item{freq}{A vector; indexed elementwise.}
\item{beta}{Passed to \code{morie_sequence_probabilities}.}
\item{gamma}{Passed to \code{morie_sequence_probabilities}.}
\item{x}{Passed to \code{morie_sequence_probabilities}.}
\item{alpha_grid}{See Usage.}
\item{y0_values}{Defaults to \code{c(0, 1)}.}
\item{link}{Passed to \code{morie_sequence_probabilities}. Defaults to \code{"logit"}.}
\item{tol}{Defaults to \code{1e-04}.}
\item{iters}{Defaults to \code{4000L}.}
}
\description{
A step of the bnshrt_native implementation. Called by \code{morie_identified_set}. See the file header for the source the module follows.
}
\value{
A list with \code{discrepancy}, \code{feasible}, \code{weights}, \code{fitted}, \code{target}.
}
\keyword{internal}
