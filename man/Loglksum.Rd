% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Loglksum}
\alias{Loglksum}
\title{Log-likelihood of independent observations (Lawson eq. 3.2)}
\usage{
Loglksum(dens) 
}
\description{
Log-likelihood of independent observations (Lawson eq. 3.2)
}
\arguments{
\item{dens}{Argument \code{dens}; see Usage.}
}
\value{
A list with \code{loglik}, \code{terms}, \code{n}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
Loglksum(V)
}
