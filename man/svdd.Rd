% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{svdd}
\alias{svdd}
\title{svdd}
\usage{
svdd(X, C = 1, kernel = "linear", gamma = 1, tol = 1e-10, max_sweeps = 500L)
}
\arguments{
\item{X}{See Usage.}
\item{C}{Defaults to \code{1}.}
\item{kernel}{Defaults to \code{"linear"}.}
\item{gamma}{Defaults to \code{1}.}
\item{tol}{Defaults to \code{1e-10}.}
\item{max_sweeps}{Defaults to \code{500L}.}
}
\description{
Part of the deepSVDD_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{alpha}, \code{center}, \code{radius2}, \code{support}, \code{outliers}, \code{kkt_violation}, \code{kernel}, \code{C}, \code{method}.
}
\keyword{internal}
\examples{
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
svdd(M)
}
