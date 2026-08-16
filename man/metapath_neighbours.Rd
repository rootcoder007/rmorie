% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{metapath_neighbours}
\alias{metapath_neighbours}
\title{metapath_neighbours}
\usage{
metapath_neighbours(edges, types, metapath)
}
\arguments{
\item{edges}{See Usage.}
\item{types}{See Usage.}
\item{metapath}{See Usage.}
}
\description{
Part of the hetgnn_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{neighbours}, \code{metapath}, \code{note}.
}
\keyword{internal}
\examples{
metapath_neighbours(edges = c(1, 2, 3, 4, 5, 6, 7, 8), types = c("a", "b", "c"), metapath = c(1, 2, 3, 4, 5, 6, 7, 8))
}
