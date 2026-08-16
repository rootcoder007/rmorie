% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.schab_adaptive_bandwidth}
\alias{.schab_adaptive_bandwidth}
\title{Mgwr/kernels.py: the n_neighbours-th order statistic, nudged by eps}
\usage{
.schab_adaptive_bandwidth(distance_row, n_neighbours, eps = 1.0000001)
}
\arguments{
\item{distance_row}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{n_neighbours}{Coerced to integer by the body, with \code{as.integer}.}
\item{eps}{Numeric; combined arithmetically in the body. Defaults to \code{1.0000001}.}
}
\description{
so the neighbour itself falls strictly inside a truncated kernel\\'s support. The regression point counts as its own first neighbour.
}
\value{
A vector, from \code{as.numeric}.
}
\keyword{internal}
\examples{
.schab_adaptive_bandwidth(distance_row = c(1, 2, 3, 4, 5, 6, 7, 8), n_neighbours = 5L)
}
