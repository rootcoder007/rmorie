% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Friedchi}
\alias{Friedchi}
\title{Chi-square approximation to Friedman Q -- Sec. 12.2, p. 442}
\usage{
Friedchi(q, k, n)
}
\description{
Chi-square approximation to Friedman Q -- Sec. 12.2, p. 442
}
\value{
A list with \code{statistic}, \code{df}, \code{p_value}, \code{mean}, \code{var_exact}, \code{var_chi2}, \code{ratio}, \code{k}, \code{n}.
}
\keyword{internal}
\examples{
Friedchi(q = 0.5, k = 5L, n = 5L)
}
