% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_tlheic}
\alias{morie_tlheic}
\title{morie_tlheic}
\usage{
morie_tlheic(psi_of_P = NULL, basis = NULL, D = NULL, score = NULL, weights = NULL, h = 1e-05, tol = 1e-04, ridge = 1e-08, mode = c("estimate", "verify", "grad", "deriv"))
}
\arguments{
\item{psi_of_P}{Defaults to \code{NULL}.}
\item{basis}{Defaults to \code{NULL}.}
\item{D}{Defaults to \code{NULL}.}
\item{score}{Defaults to \code{NULL}.}
\item{weights}{Defaults to \code{NULL}.}
\item{h}{Defaults to \code{1e-05}.}
\item{tol}{Defaults to \code{1e-04}.}
\item{ridge}{Defaults to \code{1e-08}.}
\item{mode}{Defaults to \code{c("estimate", "verify", "grad", "deriv")}.}
}
\description{
Part of the tlheic_native implementation; see the file header for the source it follows.
}
\value{
The value of \code{estimate_eic}.
}
\keyword{internal}
