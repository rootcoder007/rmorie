% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{profile_beta}
\alias{profile_beta}
\title{profile_beta}
\usage{
profile_beta(wy, wX, ar = numeric(0), ma = numeric(0), filter = "exact")
}
\arguments{
\item{wy}{See Usage.}
\item{wX}{See Usage.}
\item{ar}{Defaults to \code{numeric(0)}.}
\item{ma}{Defaults to \code{numeric(0)}.}
\item{filter}{Defaults to \code{"exact"}.}
}
\description{
Part of the sarimax_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{beta}, \code{ssq}, \code{v}, \code{f}, \code{information}, \code{sum_log_f}.
}
\keyword{internal}
\examples{
profile_beta(wy = 5L, wX = 5L)
}
