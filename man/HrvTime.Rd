% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{HrvTime}
\alias{HrvTime}
\title{SDNN, RMSSD and pNN50.  Section 2.2.5 motivates these but does not}
\usage{
HrvTime(rr)
}
\arguments{
\item{rr}{See Usage.}
}
\description{
define them; the definitions are the Task Force ones, Circulation 93(5):1043- 1065, 1996, reference [84] of the book\\'s Chapter 8.
}
\value{
A list with \code{sdnn}, \code{rmssd}, \code{nn50}, \code{pnn50}, \code{meannn}, \code{meanhr}, \code{n}, \code{units}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
HrvTime(V)
}
