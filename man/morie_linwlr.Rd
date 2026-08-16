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
\item{y}{Passed to \code{.linwlr_vec}.}
\item{A}{Passed to \code{.linwlr_vec}.}
\item{W}{Optional; may be \code{NULL}. Passed to \code{.linwlr_mat}.}
\item{propensity}{Defaults to \code{NULL}.}
\item{method}{One of \code{"gest"}, \code{"wls"}. Defaults to \code{"gest"}.}
\item{baseline}{Optional; may be \code{NULL}. Passed to \code{.linwlr_mat}.}
\item{pi_covariates}{Optional; may be \code{NULL}. Passed to \code{.linwlr_mat}.}
\item{ridge}{Numeric; combined arithmetically in the body. Defaults to \code{1e-10}.}
}
\description{
A step of the linwlr_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{result}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_linwlr(V, V)
}
