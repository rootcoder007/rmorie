% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_partial_autocorrelation}
\alias{morie_partial_autocorrelation}
\title{morie_partial_autocorrelation}
\usage{
morie_partial_autocorrelation(y, lag_max)
}
\description{
morie_partial_autocorrelation
}
\value{
A list with \code{pacf}, \code{acf}, \code{phi}, \code{lag_max}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
morie_partial_autocorrelation(y = c(1, 2, 3, 4, 5, 6, 7, 8), lag_max = 5L)
}
