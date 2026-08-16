% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{disaggregate}
\alias{disaggregate}
\title{Spread a bucket forecast back over \code{m} periods}
\usage{
disaggregate(aggregate_value, m, profile = NULL)
}
\arguments{
\item{aggregate_value}{Forecast for the bucket.}
\item{m}{Bucket size, positive integer.}
\item{profile}{Optional numeric vector of length \code{m} with   non-negative weights; when NULL equal weights are used.}
}
\description{
Equal weights assume demand is uniform within the bucket. Any supplied profile is normalised to sum to one so the disaggregated periods reconstitute the aggregate exactly; a profile that did not sum to one would silently change the total.
}
\value{
Numeric vector of length \code{m}.
}
\keyword{internal}
