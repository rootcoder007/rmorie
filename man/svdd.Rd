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
\item{X}{A matrix; indexed by row and column.}
\item{C}{Numeric; passed to \code{min}. Defaults to \code{1}.}
\item{kernel}{Coerced to character by the body, with \code{as.character}. Defaults to \code{"linear"}.}
\item{gamma}{Passed to \code{.deep_svdd_kernel}. Defaults to \code{1}.}
\item{tol}{Passed to \code{<}. Defaults to \code{1e-10}.}
\item{max_sweeps}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{500L}.}
}
\description{
A step of the deepSVDD_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{alpha}, \code{center}, \code{radius2}, \code{support}, \code{outliers}, \code{kkt_violation}, \code{kernel}, \code{C}, \code{method}.
}
\keyword{internal}
\examples{
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
svdd(M)
}
