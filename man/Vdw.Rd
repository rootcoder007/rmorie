% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Vdw}
\alias{Vdw}
\title{van der Waerden test -- Sec. 8.3.2, p. 301}
\usage{
Vdw(x, y)
}
\description{
van der Waerden test -- Sec. 8.3.2, p. 301
}
\value{
A list with \code{statistic}, \code{z}, \code{p_value}, \code{mean}, \code{var}, \code{scores}, \code{m}, \code{n}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
Vdw(V, V)
}
