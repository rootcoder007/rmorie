% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{expected_clusters}
\alias{expected_clusters}
\title{Expected number of clusters}
\usage{
expected_clusters(n, alpha)
}
\arguments{
\item{n}{Number of draws, at least 1.}
\item{alpha}{Concentration, positive.}
}
\description{
The sum \code{sum alpha/(alpha+i)} for \code{i = 0..n-1}, which grows like \code{alpha log n} -- so the concentration is not a smoothing parameter whose doubling doubles the cluster count.
}
\value{
A list with \code{expected}, \code{n}, \code{alpha},   \code{log_approximation}, \code{note}.
}
\keyword{internal}
