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
\item{adj}{A vector; indexed elementwise.}
\item{t}{Optional; may be \code{NULL}. Passed to \code{.node2v_dist}.}
\item{v}{Passed to \code{stop}.}
\item{p}{Passed to \code{alpha_pq}.}
\item{q}{Passed to \code{alpha_pq}.}
\item{weights}{Optional; may be \code{NULL}. A vector; indexed elementwise.}
}
\description{
A step of the node2v_native implementation. Called by \code{walk}. See the file header for the source the module follows.
}
\value{
A list with \code{nodes}, \code{probabilities}, \code{unnormalized}, \code{Z}.
}
\keyword{internal}
\examples{
transition_probabilities(adj = c(1, 2, 3, 4, 5, 6, 7, 8), t = c(1, 2, 3, 4, 5, 6, 7, 8), v = 5L, p = 0.5, q = 0.5)
}
