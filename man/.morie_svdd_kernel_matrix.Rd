% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_svdd_kernel_matrix}
\alias{.morie_svdd_kernel_matrix}
\title{.morie_svdd_kernel_matrix}
\usage{
.morie_svdd_kernel_matrix(X, Y = NULL, kernel = "rbf", gamma = NULL, degree = 3, coef0 = 1)
}
\arguments{
\item{X}{A matrix; indexed by row and column.}
\item{Y}{Optional; may be \code{NULL}. A matrix; indexed by row and column.}
\item{kernel}{One of \code{"linear"}, \code{"poly"}, \code{"rbf"}. Defaults to \code{"rbf"}.}
\item{gamma}{Optional; may be \code{NULL}. Numeric; combined arithmetically in the body.}
\item{degree}{Numeric; combined arithmetically in the body. Defaults to \code{3}.}
\item{coef0}{Numeric; combined arithmetically in the body. Defaults to \code{1}.}
}
\description{
A step of the svdd_native implementation. Called by \code{morie_svdd}. See the file header for the source the module follows.
}
\value{
The value of \code{K}, as built in the body.
}
\keyword{internal}
