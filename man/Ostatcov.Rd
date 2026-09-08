% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Ostatcov}
\alias{Ostatcov}
\title{Covariance of two uniform order statistics -- Sec. 2.4, p. 38}
\usage{
Ostatcov(r, s, n) 
}
\description{
Covariance of two uniform order statistics -- Sec. 2.4, p. 38
}
\arguments{
\item{r}{Argument \code{r}; see Usage.}
\item{s}{Argument \code{s}; see Usage.}
\item{n}{Argument \code{n}; see Usage.}
}
\value{
A list with \code{cov}, \code{corr}, \code{var_r}, \code{var_s}, \code{r}, \code{s}, \code{n}.
}
\keyword{internal}
\examples{
rmorie:::Ostatcov(r = 5L, s = 5L, n = 5L)
}
