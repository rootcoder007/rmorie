% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Wstnbrg}
\alias{Wstnbrg}
\title{Westenberg interquartile scale test -- eq. (9.9.1), p. 329}
\usage{
Wstnbrg(x, y)
}
\description{
Westenberg interquartile scale test -- eq. (9.9.1), p. 329
}
\value{
A list with \code{statistic}, \code{p_value}, \code{pmf}, \code{q1}, \code{q3}, \code{mean}, \code{var}, \code{m}, \code{n}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
Wstnbrg(V, V)
}
