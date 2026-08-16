% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{spatial_bias}
\alias{spatial_bias}
\title{Turn the distance matrix into the attention bias}
\usage{
spatial_bias(distance, b_table, unreachable_bias = NULL)
}
\arguments{
\item{distance}{Square integer distance matrix.}
\item{b_table}{Learnable bias table indexed by distance.}
\item{unreachable_bias}{Bias for unreachable pairs.}
}
\description{
Turn the distance matrix into the attention bias
}
\value{
List with bias, unreachable_bias, note.
}
\keyword{internal}
