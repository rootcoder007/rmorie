% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Efft2}
\alias{Efft2}
\title{Two-sample t efficacy -- eq. (13.3.9), p. 494}
\usage{
Efft2(m, n, sigma2) 
}
\description{
Two-sample t efficacy -- eq. (13.3.9), p. 494
}
\arguments{
\item{m}{Argument \code{m}; see Usage.}
\item{n}{Argument \code{n}; see Usage.}
\item{sigma2}{Argument \code{sigma2}; see Usage.}
}
\value{
A list with \code{efficacy}, \code{m}, \code{n}, \code{sigma2}.
}
\keyword{internal}
\examples{
Efft2(m = 5L, n = 5L, sigma2 = 5L)
}
