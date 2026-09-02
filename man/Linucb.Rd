% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Linucb}
\alias{Linucb}
\title{LinUCB arm scores (Li et al. 2010)}
\usage{
Linucb(x, theta, Ainv, alpha = 1) 
}
\description{
LinUCB arm scores (Li et al. 2010)
}
\arguments{
\item{x}{Argument \code{x}; see Usage.}
\item{theta}{Argument \code{theta}; see Usage.}
\item{Ainv}{Argument \code{Ainv}; see Usage.}
\item{alpha}{Argument \code{alpha}; see Usage.}
}
\value{
A list with \code{score}, \code{mean}, \code{bonus}, \code{arm}, \code{n_arms}, \code{alpha}.
}
\keyword{internal}
\examples{
Linucb(x = 5L, theta = c(1, 2, 3, 4, 5, 6, 7, 8), Ainv = c(1, 2, 3, 4, 5, 6, 7, 8))
}
