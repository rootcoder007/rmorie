% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{KMeans}
\alias{KMeans}
\title{Section 10.5.1.  WCSS falls at every step, so the iteration always}
\usage{
KMeans(X, k, maxiter = 100, tol = 1e-10, init = NULL)
}
\arguments{
\item{X}{A matrix; passed to \code{as.matrix}.}
\item{k}{See Usage.}
\item{maxiter}{Defaults to \code{100}.}
\item{tol}{Defaults to \code{1e-10}.}
\item{init}{Optional; may be \code{NULL}. A matrix; passed to \code{as.matrix}.}
}
\description{
terminates -- at a LOCAL minimum that depends on where the centroids started.  The method is unsupervised: it finds groups, and whether those groups are the diagnostic classes is a question it cannot answer.  Starting centroids are the first k distinct patterns, so the result is reproducible; random starts would return different clusterings from the same call.
}
\value{
A list with \code{labels}, \code{centroids}, \code{wcss}, \code{k}, \code{sizes}, \code{iterations}, \code{converged}, \code{local_minimum_only}, \code{depends_on_the_starting_centroids}, \code{unsupervised_groups_need_not_be_the_classes}, \code{method}.
}
\keyword{internal}
\examples{
KMeans(X = c(1, 2, 3, 4, 5, 6, 7, 8), k = 5L)
}
