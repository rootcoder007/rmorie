% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_cpads_strip_dknr}
\alias{.morie_cpads_strip_dknr}
\title{Replace CPADS-style missing codes (98, 99) with `NA`.}
\usage{
.morie_cpads_strip_dknr(x)
}
\description{
Replace CPADS-style missing codes (98, 99) with `NA`.
}
\value{
The value of \code{x}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.morie_cpads_strip_dknr(V)
}
