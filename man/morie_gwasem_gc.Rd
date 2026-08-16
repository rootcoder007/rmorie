% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_gwasem_gc}
\alias{morie_gwasem_gc}
\title{Genomic-control inflation factor}
\usage{
morie_gwasem_gc(stats, df = 1)
}
\arguments{
\item{stats}{Numeric vector of chi-square statistics.}
\item{df}{Degrees of freedom.}
}
\description{
Median observed chi-square divided by its null median. A well-calibrated analysis sits at 1.
}
\value{
Scalar.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_gwasem_gc(V)
}
