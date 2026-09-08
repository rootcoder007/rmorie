% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_tv_denoise_1d}
\alias{morie_tv_denoise_1d}
\title{morie_tv_denoise_1d}
\usage{
morie_tv_denoise_1d(signal, lam = 1, max_iter = 1000, tol = 1e-12, theta = 1) 
}
\arguments{
\item{signal}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{lam}{Numeric; combined arithmetically in the body. Defaults to \code{1}.}
\item{max_iter}{Passed to \code{morie_primal}. Defaults to \code{1000}.}
\item{tol}{Passed to \code{morie_primal}. Defaults to \code{1e-12}.}
\item{theta}{Passed to \code{morie_primal}. Defaults to \code{1}.}
}
\description{
A step of the primal_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{res}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_tv_denoise_1d(V)
}
