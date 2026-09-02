% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{XCorrCont}
\alias{XCorrCont}
\title{The continuous-time form, evaluated by the trapezoidal rule on the}
\usage{
XCorrCont(x, y, t, delays) 
}
\arguments{
\item{x}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{y}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{t}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{delays}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
samples that BOTH signals cover.  Long delays use less data, so the overlap fraction travels with the estimate.
}
\value{
A list with \code{ccf}, \code{delays}, \code{overlap_fraction}, \code{interpolated}, \code{trapezoidal}, \code{long_delays_use_less_data}, \code{method}.
}
\keyword{internal}
\examples{
XCorrCont(x = c(1, 2, 3, 4, 5, 6, 7, 8), y = c(1, 2, 3, 4, 5, 6, 7, 8), t = c(1, 2, 3, 4, 5, 6, 7, 8), delays = c(1, 2, 3, 4, 5, 6, 7, 8))
}
