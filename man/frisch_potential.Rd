% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{frisch_potential}
\alias{frisch_potential}
\title{Frisch potential}
\usage{
frisch_potential(slacks) 
}
\arguments{
\item{slacks}{Numeric vector of slack values (must be > 0).}
}
\description{
Frisch equation 5.1: \code{V = sum log(x_k)} over the slacks. Returns \code{-Inf} outside the strict interior.
}
\value{
Scalar potential, or \code{-Inf} if any entry is <= 0.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
frisch_potential(V)
}
