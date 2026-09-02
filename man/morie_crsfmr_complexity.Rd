% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_crsfmr_complexity}
\alias{morie_crsfmr_complexity}
\title{Operation counts for the two stages}
\usage{
morie_crsfmr_complexity(L, D, c) 
}
\arguments{
\item{L}{Integer, number of segments.}
\item{D}{Integer, number of dimensions.}
\item{c}{Integer, number of router vectors.}
}
\description{
Cross-time is \code{O(D L^2)}. Cross-dimension is \code{O(c D L)} through the router against \code{O(D^2 L)} all-pairs. Flattening the whole array would be \code{O(D^2 L^2)}.
}
\value{
A list of named integer counts.
}
\keyword{internal}
\examples{
morie_crsfmr_complexity(L = c(1, 2, 3, 4, 5, 6, 7, 8), D = 5L, c = c(1, 2, 3, 4, 5, 6, 7, 8))
}
