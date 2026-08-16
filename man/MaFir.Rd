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
\item{x}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{b_k}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{N}{Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.}
\item{n}{Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.}
}
\description{
used.  Equal weights are the worst choice for stopband attenuation -- a rectangle\'s sidelobes fall off slowly -- which is what the window functions of Section 3.4 exist to fix.
}
\value{
A list with \code{y}, \code{value}, \code{index}, \code{b}, \code{N}, \code{settled_from}, \code{dc_gain}, \code{equal_weights}, \code{delay_samples}, \code{method}.
}
\keyword{internal}
