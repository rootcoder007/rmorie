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
\item{signal}{See Usage.}
\item{lam}{Defaults to \code{1}.}
\item{max_iter}{Defaults to \code{1000}.}
\item{tol}{Defaults to \code{1e-12}.}
\item{theta}{Defaults to \code{1}.}
}
\description{
Part of the primal_native implementation; see the file header for the source it follows.
}
\value{
The value of \code{res}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_tv_denoise_1d(V)
}
