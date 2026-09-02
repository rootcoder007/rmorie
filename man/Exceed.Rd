% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Exceed}
\alias{Exceed}
\title{Placement / exceedance null law -- Problem 2.28(c), p. 70}
\usage{
Exceed(i, m, n, j = NULL) 
}
\description{
Placement / exceedance null law -- Problem 2.28(c), p. 70
}
\arguments{
\item{i}{Argument \code{i}; see Usage.}
\item{m}{Argument \code{m}; see Usage.}
\item{n}{Argument \code{n}; see Usage.}
\item{j}{Argument \code{j}; see Usage.}
}
\value{
A list with \code{pmf}, \code{pmf_j}, \code{cdf_j}, \code{mean}, \code{var}, \code{i}, \code{m}, \code{n}.
}
\keyword{internal}
\examples{
Exceed(i = 5L, m = 5L, n = 5L)
}
