% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{MaFir}
\alias{MaFir}
\title{Eqs (3.97)-(3.99).  With no coefficients the equal-weight boxcar is}
\usage{
MaFir(x, b_k = NULL, N = NULL, n = NULL)
}
\arguments{
\item{x}{See Usage.}
\item{b_k}{Defaults to \code{NULL}.}
\item{N}{Defaults to \code{NULL}.}
\item{n}{Defaults to \code{NULL}.}
}
\description{
used.  Equal weights are the worst choice for stopband attenuation -- a rectangle\'s sidelobes fall off slowly -- which is what the window functions of Section 3.4 exist to fix.
}
\value{
A list with \code{y}, \code{value}, \code{index}, \code{b}, \code{N}, \code{settled_from}, \code{dc_gain}, \code{equal_weights}, \code{delay_samples}, \code{method}.
}
\keyword{internal}
