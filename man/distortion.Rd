% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{distortion}
\alias{distortion}
\title{Realised pairwise distortion}
\usage{
distortion(A, E)
}
\arguments{
\item{A}{Original points, list of equal-length numeric vectors.}
\item{E}{Embedded points, one row per point of \code{A}.}
}
\description{
Measures how well \code{E} preserves the pairwise squared distances of \code{A}, over every pair, rather than reporting a probabilistic promise. Pairs whose original squared distance is zero (coincident points) are skipped, mirroring the Python arm.
}
\value{
A list with \code{worst_distortion}, \code{min_ratio},   \code{max_ratio}, \code{mean_ratio}, \code{n_pairs}.
}
\keyword{internal}
