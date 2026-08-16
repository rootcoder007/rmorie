% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Kirchidx}
\alias{Kirchidx}
\title{Kirchhoff index (Klein & Randic 1993)}
\usage{
Kirchidx(A, tol = 1e-09)
}
\description{
Kirchhoff index (Klein & Randic 1993)
}
\value{
A list with \code{Kf}, \code{Kf_spectral}, \code{n}, \code{rank}.
}
\keyword{internal}
\examples{
A <- rbind(c(0, 1, 1, 0), c(1, 0, 1, 0), c(1, 1, 0, 1), c(0, 0, 1, 0))
Kirchidx(A)
}
