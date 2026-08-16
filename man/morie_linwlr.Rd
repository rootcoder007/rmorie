% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_linwlr}
\alias{morie_linwlr}
\title{morie_linwlr}
\usage{
morie_linwlr(y, A, W = NULL, propensity = NULL, method = "gest", baseline = NULL, pi_covariates = NULL, ridge = 1e-10)
}
\arguments{
\item{y}{See Usage.}
\item{A}{See Usage.}
\item{W}{Defaults to \code{NULL}.}
\item{propensity}{Defaults to \code{NULL}.}
\item{method}{Defaults to \code{"gest"}.}
\item{baseline}{Defaults to \code{NULL}.}
\item{pi_covariates}{Defaults to \code{NULL}.}
\item{ridge}{Defaults to \code{1e-10}.}
}
\description{
Part of the linwlr_native implementation; see the file header for the source it follows.
}
\value{
The value of \code{result}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_linwlr(V, V)
}
