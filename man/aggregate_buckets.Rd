% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{aggregate_buckets}
\alias{aggregate_buckets}
\title{Sum a series into buckets of \code{m} periods}
\usage{
aggregate_buckets(y, m, overlapping = FALSE)
}
\arguments{
\item{y}{Numeric series.}
\item{m}{Bucket size, positive integer.}
\item{overlapping}{If TRUE, roll a window of width \code{m}; if   FALSE (default), partition into the last complete buckets.}
}
\description{
Non-overlapping keeps the LAST complete buckets so the most recent history is retained; overlapping produces one bucket per starting position and the resulting aggregates are autocorrelated, as the paper notes.
}
\value{
Numeric vector of bucket sums.
}
\keyword{internal}
