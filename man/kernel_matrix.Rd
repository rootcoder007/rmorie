% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{kernel_matrix}
\alias{kernel_matrix}
\title{kernel_matrix}
\usage{
kernel_matrix(X, kernel = "linear", gamma = 1, degree = 3, coef0 = 0)
}
\arguments{
\item{X}{A matrix; passed to \code{as.matrix}.}
\item{kernel}{One of \code{"linear"}, \code{"poly"}, \code{"rbf"}. Defaults to \code{"linear"}.}
\item{gamma}{Defaults to \code{1}.}
\item{degree}{Defaults to \code{3}.}
\item{coef0}{Defaults to \code{0}.}
}
\description{
A step of the svmopt_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{K}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
kernel_matrix(V)
}
