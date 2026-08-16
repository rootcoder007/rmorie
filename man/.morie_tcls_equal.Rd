% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_tcls_equal}
\alias{.morie_tcls_equal}
\title{.morie_tcls_equal}
\usage{
.morie_tcls_equal(p, q)
}
\arguments{
\item{p}{Numeric; combined arithmetically in the body.}
\item{q}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the tcls_native implementation. Called by \code{morie_emd}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.morie_tcls_equal(V, V)
}
