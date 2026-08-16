% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.lyapun_nearest_neighbours}
\alias{.lyapun_nearest_neighbours}
\title{.lyapun_nearest_neighbours}
\usage{
.lyapun_nearest_neighbours(pts, min_sep)
}
\arguments{
\item{pts}{A vector; its length is taken and its elements indexed.}
\item{min_sep}{Passed to \code{<=}.}
}
\description{
A step of the lyapun_native implementation. Called by \code{divergence_curve}. See the file header for the source the module follows.
}
\value{
A list with \code{nn}, \code{d0}.
}
\keyword{internal}
\examples{
.lyapun_nearest_neighbours(pts = c(1, 2, 3, 4, 5, 6, 7, 8), min_sep = 5L)
}
