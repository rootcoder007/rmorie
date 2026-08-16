% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{FdVag}
\alias{FdVag}
\title{Sections 6.6.2-6.6.3: PSA is the book\'s preferred FD estimator for a}
\usage{
FdVag(x, fs, fmin = 100, fmax = 500, nperseg = NULL)
}
\arguments{
\item{x}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{fs}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{fmin}{Defaults to \code{100}.}
\item{fmax}{Defaults to \code{500}.}
\item{nperseg}{Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.}
}
\description{
self-affine signal, applied to knee-joint VAG signals.  The band is an argument because it decides the answer -- a band that reaches down into baseline drift fits the drift\'s slope, not the signal\'s.
}
\value{
The value of \code{r}, as built in the body.
}
\keyword{internal}
