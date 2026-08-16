% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{estimate_eic}
\alias{estimate_eic}
\title{estimate_eic}
\usage{
estimate_eic(psi_of_P, basis, weights = NULL, h = 1e-05, ridge = 1e-08)
}
\arguments{
\item{psi_of_P}{See Usage.}
\item{basis}{See Usage.}
\item{weights}{Defaults to \code{NULL}.}
\item{h}{Defaults to \code{1e-05}.}
\item{ridge}{Defaults to \code{1e-08}.}
}
\description{
Part of the tlheic_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{D}, \code{coefficients}, \code{n_directions}, \code{mean}, \code{method}, \code{note}.
}
\keyword{internal}
