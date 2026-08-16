% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{verify_gradient}
\alias{verify_gradient}
\title{verify_gradient}
\usage{
verify_gradient(psi_of_P, D, score, weights = NULL, h = 1e-05, tol = 1e-04)
}
\arguments{
\item{psi_of_P}{See Usage.}
\item{D}{A vector; its length is taken.}
\item{score}{See Usage.}
\item{weights}{Defaults to \code{NULL}.}
\item{h}{Defaults to \code{1e-05}.}
\item{tol}{Defaults to \code{1e-04}.}
}
\description{
A step of the tlheic_native implementation. Called by \code{morie_tlheic}. See the file header for the source the module follows.
}
\value{
A list with \code{derivative}, \code{inner_product}, \code{difference}, \code{verified}, \code{note}.
}
\keyword{internal}
