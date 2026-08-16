% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Effwsr}
\alias{Effwsr}
\title{Signed-rank efficacy -- eq. (13.3.4), p. 490}
\usage{
Effwsr(n, f0, integral)
}
\description{
Signed-rank efficacy -- eq. (13.3.4), p. 490
}
\value{
A list with \code{efficacy}, \code{limit}, \code{integral}, \code{n}.
}
\keyword{internal}
\examples{
Effwsr(n = 5L, f0 = c(1, 2, 3, 4, 5, 6, 7, 8), integral = c(1, 2, 3, 4, 5, 6, 7, 8))
}
