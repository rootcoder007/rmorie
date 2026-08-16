% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.lyapun_linear_region}
\alias{.lyapun_linear_region}
\title{.lyapun_linear_region}
\usage{
.lyapun_linear_region(curve, lo_frac = 0.1, hi_frac = 0.8)
}
\arguments{
\item{curve}{A vector; its length is taken and its elements indexed.}
\item{lo_frac}{Numeric; combined arithmetically in the body. Defaults to \code{0.1}.}
\item{hi_frac}{Numeric; combined arithmetically in the body. Defaults to \code{0.8}.}
}
\description{
A step of the lyapun_native implementation. Called by \code{lyapunov_exponent}. See the file header for the source the module follows.
}
\value{
A list with \code{lo}, \code{hi}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.lyapun_linear_region(V)
}
