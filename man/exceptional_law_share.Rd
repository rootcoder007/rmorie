% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{exceptional_law_share}
\alias{exceptional_law_share}
\title{Share of subjects whose blip sits within tol of zero}
\usage{
exceptional_law_share(blips, tol = 0.01)
}
\arguments{
\item{blips}{Blip vector.}
\item{tol}{Tolerance.}
}
\description{
Share of subjects whose blip sits within tol of zero
}
\value{
Scalar in [0,1].
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
exceptional_law_share(V)
}
