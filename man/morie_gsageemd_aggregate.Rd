% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_gsageemd_aggregate}
\alias{morie_gsageemd_aggregate}
\title{morie_gsageemd_aggregate}
\usage{
morie_gsageemd_aggregate(vectors, how = "mean", W = NULL) 
}
\arguments{
\item{vectors}{Passed to \code{.gsageemd_mat}.}
\item{how}{One of \code{"max_pool"}, \code{"mean"}. Defaults to \code{"mean"}.}
\item{W}{Optional; may be \code{NULL}. A matrix; indexed by row and column.}
}
\description{
A step of the gsageemd_native implementation. Called by \code{sage_layer}. See the file header for the source the module follows.
}
\value{
One of two values, depending on the branch taken.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_gsageemd_aggregate(V)
}
