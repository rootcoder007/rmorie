% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{alpha_pq}
\alias{alpha_pq}
\title{alpha_pq}
\usage{
alpha_pq(d_tx, p, q) 
}
\arguments{
\item{d_tx}{Coerced to integer by the body, with \code{as.integer}.}
\item{p}{Passed to \code{node2v_check_pq}.}
\item{q}{Passed to \code{node2v_check_pq}.}
}
\description{
A step of the node2v_native implementation. Called by \code{transition_probabilities}. See the file header for the source the module follows.
}
\value{
Nothing; this branch always raises.
}
\keyword{internal}
\examples{
c(alpha_pq(0L, 1, 1), alpha_pq(1L, 1, 2), alpha_pq(2L, 1, 0.5))
}
