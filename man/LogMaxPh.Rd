% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{LogMaxPh}
\alias{LogMaxPh}
\title{Eq (4.71): log(1 - beta z) = -sum beta^n/n z^n, |z| < 1/|beta|.  The}
\usage{
LogMaxPh(beta, terms = 20, z = NULL)
}
\arguments{
\item{beta}{See Usage.}
\item{terms}{Defaults to \code{20}.}
\item{z}{Defaults to \code{NULL}.}
}
\description{
mirror of eq (4.70): positive powers of z, so the maximum-phase part of the cepstrum is anticausal -- which is why liftering windows for homomorphic deconvolution are two-sided.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
