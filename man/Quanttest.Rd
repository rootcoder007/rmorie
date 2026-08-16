% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Quanttest}
\alias{Quanttest}
\title{Distribution-free quantile test -- Sec. 5.3, p. 163}
\usage{
Quanttest(x, q0, p = 0.5, alternative = "two-sided")
}
\description{
Distribution-free quantile test -- Sec. 5.3, p. 163
}
\value{
A list with \code{statistic}, \code{p_value}, \code{n}, \code{p}, \code{mean}, \code{var}, \code{alternative}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
Quanttest(V, V)
}
