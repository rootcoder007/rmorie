% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Signp}
\alias{Signp}
\title{Exact sign-test p-value -- eq. (5.4.3), p. 169}
\usage{
Signp(k, n, alternative = "two-sided")
}
\description{
Exact sign-test p-value -- eq. (5.4.3), p. 169
}
\value{
A list with \code{p_value}, \code{p_lower}, \code{p_upper}, \code{statistic}, \code{n}, \code{alternative}.
}
\keyword{internal}
\examples{
Signp(k = 5L, n = 5L)
}
