% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{kernel_hac}
\alias{kernel_hac}
\title{kernel_hac}
\usage{
kernel_hac(v, bandwidth, kernel = "qs", n_params = 0, n = NULL)
}
\arguments{
\item{v}{A matrix; passed to \code{as.matrix}.}
\item{bandwidth}{See Usage.}
\item{kernel}{Passed to \code{.check_kernel}. Defaults to \code{"qs"}.}
\item{n_params}{Numeric; combined arithmetically in the body. Defaults to \code{0}.}
\item{n}{Defaults to \code{NULL}.}
}
\description{
A step of the andmnh_native implementation. Called by \code{andrews_monahan_hac}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
kernel_hac(v = c(1, 2, 3, 4, 5, 6, 7, 8), bandwidth = 0.5)
}
