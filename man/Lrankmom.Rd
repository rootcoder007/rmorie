% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Lrankmom}
\alias{Lrankmom}
\title{Moments of a linear rank statistic (Gibbons & Chakraborti Thm 7.3.1-7.3.2)}
\usage{
Lrankmom(a, m)
}
\description{
Moments of a linear rank statistic (Gibbons & Chakraborti Thm 7.3.1-7.3.2)
}
\value{
A list with \code{mean}, \code{variance}, \code{se}, \code{score_mean}, \code{N}, \code{m}, \code{n}.
}
\keyword{internal}
\examples{
Lrankmom(a = c(1, 2, 3, 4, 5, 6, 7, 8), m = 5L)
}
