% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_spx_sarneg2}
\alias{.morie_spx_sarneg2}
\title{.morie_spx_sarneg2}
\usage{
.morie_spx_sarneg2(y, X, W, rho)
}
\arguments{
\item{y}{A vector; its length is taken.}
\item{X}{Passed to \code{.morie_spx_matmul}.}
\item{W}{Numeric; combined arithmetically in the body.}
\item{rho}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the sp_fill implementation. Called by \code{SpErrMod}. See the file header for the source the module follows.
}
\value{
A list with \code{v}, \code{b}, \code{s2}.
}
\keyword{internal}
\examples{
.morie_spx_sarneg2(y = c(1, 2, 3, 4, 5, 6, 7, 8), X = c(1, 2, 3, 4, 5, 6, 7, 8), W = c(1, 2, 3, 4, 5, 6, 7, 8), rho = 0.5)
}
