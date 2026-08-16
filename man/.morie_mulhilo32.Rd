% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_mulhilo32}
\alias{.morie_mulhilo32}
\title{Exact 32x32 -> 64 bit product, returned as (hi, lo) 32-bit halves.}
\usage{
.morie_mulhilo32(a, b)
}
\arguments{
\item{a}{Numeric; combined arithmetically in the body.}
\item{b}{Numeric; combined arithmetically in the body.}
}
\description{
The full product can reach 2^64, past the 2^53 where doubles stop being exact integers, so it is assembled from 16-bit partial products.
}
\value{
A list with \code{hi}, \code{lo}.
}
\keyword{internal}
