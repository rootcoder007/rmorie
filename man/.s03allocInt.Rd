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
\item{alloc}{See Usage.}
\item{m0}{See Usage.}
}
\description{
Deterministic: the units left over after flooring go to the largest fractional parts, ties broken by the lower stratum index, so both language arms land on the same integers.
}
\value{
The value of \code{base}, as built in the body.
}
\keyword{internal}
