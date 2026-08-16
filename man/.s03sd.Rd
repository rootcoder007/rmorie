% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.s03sd}
\alias{.s03sd}
\title{.s03sd}
\usage{
.s03sd(v, ddof = 1L)
}
\arguments{
\item{v}{Passed to \code{.s03var}.}
\item{ddof}{Passed to \code{.s03var}. Defaults to \code{1L}.}
}
\description{
A step of the helpers_s03 implementation. Called by \code{Btbayes}, \code{Btcbb}, \code{Btcicor} and 28 others in the module. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.s03sd(V)
}
