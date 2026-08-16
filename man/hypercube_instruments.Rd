% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{hypercube_instruments}
\alias{hypercube_instruments}
\title{hypercube_instruments}
\usage{
hypercube_instruments(X, n_levels = 3L)
}
\arguments{
\item{X}{See Usage.}
\item{n_levels}{Defaults to \code{3L}.}
}
\description{
A step of the bnskmt_native implementation. Called by \code{ks_confidence_set}. See the file header for the source the module follows.
}
\value{
A list with \code{instruments}, \code{n_instruments}, \code{n_levels}.
}
\keyword{internal}
\examples{
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
hypercube_instruments(M)
}
