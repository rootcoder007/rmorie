% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Mwuci}
\alias{Mwuci}
\title{Mann-Whitney shift CI -- Sec. 6.6.2, p. 267}
\usage{
Mwuci(x, y, k) 
}
\description{
Mann-Whitney shift CI -- Sec. 6.6.2, p. 267
}
\arguments{
\item{x}{Argument \code{x}; see Usage.}
\item{y}{Argument \code{y}; see Usage.}
\item{k}{Argument \code{k}; see Usage.}
}
\value{
A list with \code{lower}, \code{upper}, \code{estimate}, \code{k}, \code{ndiff}, \code{m}, \code{n}.
}
\keyword{internal}
\examples{
Mwuci(x = c(1, 2, 3, 4, 5, 6, 7, 8), y = c(1, 2, 3, 4, 5, 6, 7, 8), k = 5L)
}
