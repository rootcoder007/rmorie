% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Ostatmom}
\alias{Ostatmom}
\title{Moments of a uniform order statistic -- Sec. 2.4, p. 38}
\usage{
Ostatmom(r, n, k = 1) 
}
\description{
Moments of a uniform order statistic -- Sec. 2.4, p. 38
}
\arguments{
\item{r}{Argument \code{r}; see Usage.}
\item{n}{Argument \code{n}; see Usage.}
\item{k}{Argument \code{k}; see Usage.}
}
\value{
A list with \code{moment}, \code{mean}, \code{var}, \code{r}, \code{n}, \code{k}.
}
\keyword{internal}
\examples{
Ostatmom(r = 5L, n = 5L)
}
