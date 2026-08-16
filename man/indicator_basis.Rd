% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{indicator_basis}
\alias{indicator_basis}
\title{indicator_basis}
\usage{
indicator_basis(X, knots = NULL, max_order = 2L)
}
\arguments{
\item{X}{A matrix; passed to \code{as.matrix}.}
\item{knots}{Optional; may be \code{NULL}. A matrix; passed to \code{as.matrix}.}
\item{max_order}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{2L}.}
}
\description{
A step of the tlhal_native implementation. Called by \code{hal_fit}. See the file header for the source the module follows.
}
\value{
A list with \code{design}, \code{columns}, \code{n_basis}, \code{max_order}.
}
\keyword{internal}
