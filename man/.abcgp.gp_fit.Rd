% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.abcgp.gp_fit}
\alias{.abcgp.gp_fit}
\title{.abcgp.gp_fit}
\usage{
.abcgp.gp_fit(design, values, nugget = NULL, lengthscale = NULL, kernel = "sqexp", tau2 = NULL)
}
\arguments{
\item{design}{See Usage.}
\item{values}{See Usage.}
\item{nugget}{Defaults to \code{NULL}.}
\item{lengthscale}{Defaults to \code{NULL}.}
\item{kernel}{Defaults to \code{"sqexp"}.}
\item{tau2}{Defaults to \code{NULL}.}
}
\description{
Part of the abcgp_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{design}, \code{values}, \code{beta}, \code{tau2}, \code{lengthscale}, \code{kernel}, \code{nugget}, \code{chol}, \code{Ainv_r}, \code{Ainv_H}, \code{H}, \code{HtAinvH_chol}, \code{n}, \code{q}, \code{dim}.
}
\keyword{internal}
