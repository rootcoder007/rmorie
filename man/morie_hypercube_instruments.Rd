% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_hypercube_instruments}
\alias{morie_hypercube_instruments}
\title{morie_hypercube_instruments}
\usage{
morie_hypercube_instruments(X, n_levels = 3L)
}
\arguments{
\item{X}{A matrix; passed to \code{as.matrix}.}
\item{n_levels}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{3L}.}
}
\description{
A step of the bndsmw_native implementation. Called by \code{morie_confidence_set}. See the file header for the source the module follows.
}
\value{
A list with \code{instruments}, \code{n_instruments}, \code{n_levels}, \code{note}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_hypercube_instruments(V)
}
