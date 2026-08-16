% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{transition_probabilities}
\alias{transition_probabilities}
\title{transition_probabilities}
\usage{
transition_probabilities(adj, t, v, p, q, weights = NULL)
}
\arguments{
\item{adj}{See Usage.}
\item{t}{See Usage.}
\item{v}{See Usage.}
\item{p}{See Usage.}
\item{q}{See Usage.}
\item{weights}{Defaults to \code{NULL}.}
}
\description{
Part of the node2v_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{nodes}, \code{probabilities}, \code{unnormalized}, \code{Z}.
}
\keyword{internal}
\examples{
transition_probabilities(adj = c(1, 2, 3, 4, 5, 6, 7, 8), t = c(1, 2, 3, 4, 5, 6, 7, 8), v = 5L, p = 0.5, q = 0.5)
}
