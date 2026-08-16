% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_rdd_lp_nn}
\alias{.morie_rdd_lp_nn}
\title{Internal helper: one-sided local polynomial fit at the cutoff}
\usage{
.morie_rdd_lp_nn(x, y, cutoff, h, p = 1, kernel = "triangular", nn_J = 3L)
}
\description{
Weighted least squares of y on ((x - c)^0 ... (x - c)^p) using kernel weights K((x - c)/h), on one side of the cutoff. Returns the full coefficient vector and the nearest-neighbor (J = 3) heteroskedasticity-robust covariance (the "conventional" variance of rdrobust with vce = "nn").
}
\value{
A list with \code{beta}, \code{vcov}, \code{n}.
}
\keyword{internal}
