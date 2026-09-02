% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Intmod}
\alias{Intmod}
\title{Modulated point-process intensity (Lawson eq. 6.3)}
\usage{
Intmod(lam0, lam1) 
}
\description{
Modulated point-process intensity (Lawson eq. 6.3)
}
\arguments{
\item{lam0}{Argument \code{lam0}; see Usage.}
\item{lam1}{Argument \code{lam1}; see Usage.}
}
\value{
A list with \code{intensity}, \code{total}, \code{n}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
Intmod(V, V)
}
