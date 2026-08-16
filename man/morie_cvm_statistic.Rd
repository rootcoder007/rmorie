% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_cvm_statistic}
\alias{morie_cvm_statistic}
\title{morie_cvm_statistic}
\usage{
morie_cvm_statistic(m, instruments, form = "sum", n_equality = 0L, weights = NULL)
}
\arguments{
\item{m}{See Usage.}
\item{instruments}{See Usage.}
\item{form}{Defaults to \code{"sum"}.}
\item{n_equality}{Defaults to \code{0L}.}
\item{weights}{Defaults to \code{NULL}.}
}
\description{
Part of the bndsmw_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{statistic}, \code{per_instrument}, \code{form}, \code{n_instruments}, \code{method}.
}
\keyword{internal}
\examples{
D <- data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9))
morie_cvm_statistic(D, D)
}
