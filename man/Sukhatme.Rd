% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Sukhatme}
\alias{Sukhatme}
\title{Sukhatme scale test -- eq. (9.7.1), p. 323}
\usage{
Sukhatme(x, y, alternative = "two-sided")
}
\description{
Sukhatme scale test -- eq. (9.7.1), p. 323
}
\value{
A list with \code{statistic}, \code{mean}, \code{var}, \code{z}, \code{p_value}, \code{phat}, \code{m}, \code{n}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
Sukhatme(V, V)
}
