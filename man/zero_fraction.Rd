% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{zero_fraction}
\alias{zero_fraction}
\title{Share of non-positive periods}
\usage{
zero_fraction(y) 
}
\arguments{
\item{y}{Numeric series of demand observations.}
}
\description{
The zero share that aggregation is meant to reduce; this is the quantity the paper calls the self-improving mechanism.
}
\value{
The fraction of periods with no positive demand.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
zero_fraction(V)
}
