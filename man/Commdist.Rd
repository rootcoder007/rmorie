% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Commdist}
\alias{Commdist}
\title{Commute-time distance (Klein & Randic 1993; Chandra et al. 1989)}
\usage{
Commdist(A, tol = 1e-09)
}
\description{
Commute-time distance (Klein & Randic 1993; Chandra et al. 1989)
}
\value{
A list with \code{C}, \code{R}, \code{two_m}, \code{n}.
}
\keyword{internal}
\examples{
A <- rbind(c(0, 1, 1, 0), c(1, 0, 1, 0), c(1, 1, 0, 1), c(0, 0, 1, 0))
Commdist(A)
}
