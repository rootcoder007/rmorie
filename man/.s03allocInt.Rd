% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.s03allocInt}
\alias{.s03allocInt}
\title{Round a real allocation to integers summing to round(m0).}
\usage{
.s03allocInt(alloc, m0)
}
\arguments{
\item{alloc}{A vector; its length is taken.}
\item{m0}{Passed to \code{round}.}
}
\description{
Deterministic: the units left over after flooring go to the largest fractional parts, ties broken by the lower stratum index, so both language arms land on the same integers.
}
\value{
The value of \code{base}, as built in the body.
}
\keyword{internal}
\examples{
.s03allocInt(alloc = c(1, 2, 3, 4, 5, 6, 7, 8), m0 = 5L)
}
