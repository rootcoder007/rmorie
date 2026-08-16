% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{CircConv}
\alias{CircConv}
\title{Eq (3.90): y_p(n) = sum_k x_p(k) h_p[(n-k) mod N], defined only for}
\usage{
CircConv(x, h, npoints = NULL)
}
\arguments{
\item{x}{See Usage.}
\item{h}{See Usage.}
\item{npoints}{Defaults to \code{NULL}.}
}
\description{
equal periods.  Both routes -- the modular sum and the inverse DFT of X(k)H(k) -- are computed; their agreement is eq (3.87) at equal N.
}
\value{
A list with \code{y}, \code{via_dft}, \code{N}, \code{max_difference}, \code{agrees}, \code{equals_linear}, \code{linear_length}, \code{method}.
}
\keyword{internal}
