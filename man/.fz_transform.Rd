% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.fz_transform}
\alias{.fz_transform}
\title{A bijection g from the whole line onto the support, with inverse}
\usage{
.fz_transform(kind = "log")
}
\arguments{
\item{kind}{The body requires: kind must be 'log' or 'identity'. Defaults to \code{"log"}.}
}
\description{
and first two derivatives. The derivatives are what appear in the bias coefficients b_1, b_2, b_3 of (4.14), (4.15) and (4.21): the transformation does not remove the bias, it makes it computable and O(h^2) everywhere including at the edge.
}
\value{
Nothing; this branch always raises.
}
\keyword{internal}
