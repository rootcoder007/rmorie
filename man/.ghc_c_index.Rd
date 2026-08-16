% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.ghc_c_index}
\alias{.ghc_c_index}
\title{Internal: Harrell\\'s concordance index in base R, mirroring}
\usage{
.ghc_c_index(times, events, risks)
}
\arguments{
\item{times}{A vector; its length is taken and its elements indexed.}
\item{events}{A vector; its length is taken and its elements indexed.}
\item{risks}{A vector; its length is taken and its elements indexed.}
}
\description{
morie.fn.survrsf.c_index. Counts over comparable pairs (i, j) where the earlier observation is an event; non-events at the shorter time do not contribute because their true event time is unknown.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
.ghc_c_index(times = c(1, 2, 3, 4, 5, 6, 7, 8), events = c(1, 2, 3, 4, 5, 6, 7, 8), risks = c(1, 2, 3, 4, 5, 6, 7, 8))
}
