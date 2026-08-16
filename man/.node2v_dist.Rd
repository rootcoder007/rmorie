% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.node2v_dist}
\alias{.node2v_dist}
\title{Internal helper: shortest-path distance from t to x in the unweighted}
\usage{
.node2v_dist(adj, t, x)
}
\arguments{
\item{adj}{A vector; indexed elementwise.}
\item{t}{Passed to \code{identical}.}
\item{x}{Coerced to character by the body, with \code{as.character}.}
}
\description{
adjacency dict, restricted to 0/1/2.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
.node2v_dist(adj = c(1, 2, 3, 4, 5, 6, 7, 8), t = c(1, 2, 3, 4, 5, 6, 7, 8), x = c(1, 2, 3, 4, 5, 6, 7, 8))
}
