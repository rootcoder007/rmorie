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
\item{X}{See Usage.}
\item{Y}{Defaults to \code{NULL}.}
\item{kernel}{Defaults to \code{"rbf"}.}
\item{gamma}{Defaults to \code{NULL}.}
\item{degree}{Defaults to \code{3}.}
\item{coef0}{Defaults to \code{1}.}
}
\description{
Part of the svdd_native implementation; see the file header for the source it follows.
}
\value{
The value of \code{K}, as built in the body.
}
\keyword{internal}
