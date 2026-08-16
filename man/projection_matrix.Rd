% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{projection_matrix}
\alias{projection_matrix}
\title{Random projection matrix with binary coins}
\usage{
projection_matrix(d, k, distribution = "rademacher", seed = 0L)
}
\arguments{
\item{d}{Source dimension, positive integer.}
\item{k}{Target dimension, positive integer.}
\item{distribution}{One of \code{"rademacher"}, \code{"sparse"}.}
\item{seed}{Seed for the shared generator.}
}
\description{
A \code{d}-by-\code{k} matrix whose entries are drawn from the chosen Achlioptas distribution using the shared SplitMix64 generator, one uniform per cell in row-major order, so the R arm and the Python arm consume the same stream in the same order.
}
\value{
A length-\code{d} list whose elements are length-\code{k}   numeric vectors; together they form the matrix \code{R}.
}
\keyword{internal}
