% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{cbs_statistic}
\alias{cbs_statistic}
\title{Circular binary segmentation statistic}
\usage{
cbs_statistic(x) 
}
\arguments{
\item{x}{numeric vector of log-ratio intensities along a chromosome.}
}
\description{
The Z_C statistic of Olshen et al. (2004) section 2: splice the segment into a circle and test the arc i+1..j against its complement.
}
\value{
list with `z` (Z_C), `i` and `j` (the maximising half-open         arc indices, 0-based).
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
cbs_statistic(V)
}
