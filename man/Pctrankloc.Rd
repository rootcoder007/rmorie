% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Pctrankloc}
\alias{Pctrankloc}
\title{Percentile modified rank test for location -- eqs. (8.3.5)-(8.3.6), p. 304}
\usage{
Pctrankloc(x, y, s = 0.5, r = NULL)
}
\description{
Percentile modified rank test for location -- eqs. (8.3.5)-(8.3.6), p. 304
}
\value{
A list with \code{statistic}, \code{tupper}, \code{blower}, \code{var}, \code{var_book}, \code{z}, \code{p_value}, \code{S}, \code{R}, \code{m}, \code{n}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
Pctrankloc(V, V)
}
