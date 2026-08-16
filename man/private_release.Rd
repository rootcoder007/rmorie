% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{private_release}
\alias{private_release}
\title{Release f(D) + Lap(Delta f / epsilon)}
\usage{
private_release(value, sensitivity, epsilon, seed = 0)
}
\arguments{
\item{value}{The non-private statistic.}
\item{sensitivity}{L1 sensitivity.}
\item{epsilon}{Privacy budget.}
\item{seed}{Seed for the shared generator.}
}
\description{
Release f(D) + Lap(Delta f / epsilon)
}
\value{
A list with \code{released}, \code{noise}, \code{scale},   \code{epsilon}, \code{noise_variance}, \code{note}.
}
\keyword{internal}
\examples{
r <- private_release(value = 0.4, sensitivity = 0.02, epsilon = 1)
str(r, max.level = 1)
}
