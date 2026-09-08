% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Lgcpint}
\alias{Lgcpint}
\title{Log-Gaussian Cox process intensity (Lawson eq. 6.18)}
\usage{
Lgcpint(lam0, beta, S) 
}
\description{
Log-Gaussian Cox process intensity (Lawson eq. 6.18)
}
\arguments{
\item{lam0}{Argument \code{lam0}; see Usage.}
\item{beta}{Argument \code{beta}; see Usage.}
\item{S}{Argument \code{S}; see Usage.}
}
\value{
A list with \code{intensity}, \code{total}, \code{beta}, \code{n}.
}
\keyword{internal}
\examples{
rmorie:::Lgcpint(lam0 = c(1, 2, 3, 4, 5, 6, 7, 8), beta = 0.5, S = c(1, 2, 3, 4, 5, 6, 7, 8))
}
