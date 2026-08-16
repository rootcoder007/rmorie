% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.k03_lcg}
\alias{.k03_lcg}
\title{# One MINSTD step. Exact in double precision (a * m < 2^53), so both}
\usage{
.k03_lcg(state)
}
\arguments{
\item{state}{Numeric; combined arithmetically in the body.}
}
\description{
# language arms produce bit-identical initial weights.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.k03_lcg(V)
}
