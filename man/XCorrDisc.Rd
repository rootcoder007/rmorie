% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{XCorrDisc}
\alias{XCorrDisc}
\title{The RAW sum, not divided by N: the matched-filter output AT each}
\usage{
XCorrDisc(x, y, delays = NULL)
}
\arguments{
\item{x}{See Usage.}
\item{y}{See Usage.}
\item{delays}{Optional; may be \code{NULL}. A vector; its length is taken.}
}
\description{
instant IS this sum, so its peak locates the pattern.  Neither XCorr mode gives it (biased divides by n, unbiased by the overlap count), so it is computed here rather than delegated.
}
\value{
A list with \code{ccf}, \code{lags}, \code{peak}, \code{peak_lag}, \code{normalized}, \code{is_the_matched_filter_output}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
XCorrDisc(V, V)
}
