% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{alpha_ar1}
\alias{alpha_ar1}
\title{alpha_ar1}
\usage{
alpha_ar1(v, q = 2, weights = NULL)
}
\arguments{
\item{v}{A matrix; passed to \code{as.matrix}.}
\item{q}{Defaults to \code{2}.}
\item{weights}{Optional; may be \code{NULL}. Compared against \code{"drop_first"}.}
}
\description{
A step of the andmnh_native implementation. Called by \code{automatic_bandwidth}. See the file header for the source the module follows.
}
\value{
A list with \code{alpha}, \code{fits}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
alpha_ar1(V)
}
