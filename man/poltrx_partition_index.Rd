% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{poltrx_partition_index}
\alias{poltrx_partition_index}
\title{Partition index of a point}
\usage{
poltrx_partition_index(x, level, lo = 0, hi = 1)
}
\arguments{
\item{x}{Point at which to evaluate.}
\item{level}{Level \code{m}, integer \code{>= 1}.}
\item{lo}{Lower bound of the partitioned interval.}
\item{hi}{Upper bound of the partitioned interval.}
}
\description{
Returns the binary string, which IS the address in the tree.
}
\value{
A list with \code{epsilon}, \code{interval} and   \code{level}.
}
\keyword{internal}
