% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Pageasymp}
\alias{Pageasymp}
\title{Page\'s L normal approximation -- eq. (12.3.2), p. 449}
\usage{
Pageasymp(ell, k, n, correct = TRUE)
}
\description{
Page\'s L normal approximation -- eq. (12.3.2), p. 449
}
\value{
A list with \code{z}, \code{p_value}, \code{mean}, \code{var}, \code{statistic}, \code{k}, \code{n}.
}
\keyword{internal}
\examples{
Pageasymp(ell = c(1, 2, 3, 4, 5, 6, 7, 8), k = 5L, n = 5L)
}
