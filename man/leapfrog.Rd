% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{leapfrog}
\alias{leapfrog}
\title{One leapfrog step: half-kick, drift, half-kick. Reversible and}
\usage{
leapfrog(theta, r, eps, grad)
}
\arguments{
\item{theta}{See Usage.}
\item{r}{See Usage.}
\item{eps}{See Usage.}
\item{grad}{See Usage.}
}
\description{
volume-preserving, which lets a single Metropolis test on the energy correct the discretisation error exactly.
}
\value{
A list with \code{theta}, \code{r}.
}
\keyword{internal}
