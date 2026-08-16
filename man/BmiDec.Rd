% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{BmiDec}
\alias{BmiDec}
\title{BmiDec}
\usage{
BmiDec(y, C, a = NULL, procnoise = 1e-04, obsnoise = 0.01, p0 = 0.01)
}
\arguments{
\item{y}{Passed to \code{.morie_bx_mat}.}
\item{C}{Passed to \code{.morie_bx_mat}.}
\item{a}{Optional; may be \code{NULL}. Passed to \code{.morie_bx_mat}.}
\item{procnoise}{Defaults to \code{1e-04}.}
\item{obsnoise}{Defaults to \code{0.01}.}
\item{p0}{Defaults to \code{0.01}.}
}
\description{
A step of the rangayyan_class2 implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{states}, \code{innovations}, \code{gain}, \code{predicted}, \code{method}.
}
\keyword{internal}
\examples{
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
D <- data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9))
BmiDec(D, M)
}
