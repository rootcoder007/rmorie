% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.binary_ci}
\alias{.binary_ci}
\title{Internal helper: Binary Ci}
\usage{
.binary_ci(successes, n)
}
\description{
Internal helper: Binary Ci
}
\value{
A list with \code{p}, \code{se}, \code{ci}.
}
\keyword{internal}
\examples{
.binary_ci(successes = c(1, 2, 3, 4, 5, 6, 7, 8), n = 5L)
}
