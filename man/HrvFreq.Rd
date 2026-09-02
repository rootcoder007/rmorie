% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{HrvFreq}
\alias{HrvFreq}
\title{Section 8.12.  Task Force bands by default; Bianchi et al. bands on}
\usage{
HrvFreq(rr, fsr = 4, bands = "taskforce") 
}
\arguments{
\item{rr}{A vector; its length is taken and its elements indexed.}
\item{fsr}{Numeric; combined arithmetically in the body. Defaults to \code{4}.}
\item{bands}{The body requires: bands must be 'taskforce' or 'bianchi'. Defaults to \code{"taskforce"}.}
}
\description{
request.  The RR series is a series of EVENTS, so it is interpolated onto a uniform grid before the periodogram; the mean is removed first so DC does not leak into VLF.
}
\value{
A list with \code{vlf}, \code{lf}, \code{hf}, \code{total}, \code{vlfpct}, \code{lfpct}, \code{hfpct}, \code{lfhf}, \code{bands}, \code{limits}, \code{fsr}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
HrvFreq(V)
}
