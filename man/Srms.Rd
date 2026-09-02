% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Srms}
\alias{Srms}
\title{Eqs (3.8)-(3.10): MS, RMS, SD.  The divisor is N in all three; eq}
\usage{
Srms(x) 
}
\arguments{
\item{x}{Passed to \code{.morie_rg_aslist}.}
}
\description{
(3.10) is the population SD, not the N-1 unbiased one.
}
\value{
A list with \code{rms}, \code{ms}, \code{sd}, \code{mean}, \code{n}, \code{ddof}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
Srms(V)
}
