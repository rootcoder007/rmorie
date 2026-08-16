% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.weighted_binary_estimate}
\alias{.weighted_binary_estimate}
\title{Internal helper: Weighted Binary Estimate}
\usage{
.weighted_binary_estimate(x, w)
}
\description{
Internal helper: Weighted Binary Estimate
}
\value{
A list with \code{p}, \code{se}, \code{ci}, \code{n}, \code{n_eff}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.weighted_binary_estimate(V, V)
}
