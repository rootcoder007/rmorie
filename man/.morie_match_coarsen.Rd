% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_match_coarsen}
\alias{.morie_match_coarsen}
\title{Internal helper: CEM coarsening}
\usage{
.morie_match_coarsen(x, bins)
}
\description{
Internal helper: CEM coarsening
}
\value{
A character value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
D <- data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9))
.morie_match_coarsen(D, V)
}
