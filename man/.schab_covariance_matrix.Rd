% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.schab_covariance_matrix}
\alias{.schab_covariance_matrix}
\title{Sigma(theta) for the model of Sec. 4.3. C(0) = c0 + sigma0^2 and}
\usage{
.schab_covariance_matrix(coords, nugget, sill, rng, model)
}
\arguments{
\item{coords}{A matrix; passed to \code{as.matrix}.}
\item{nugget}{Numeric; combined arithmetically in the body.}
\item{sill}{Numeric; combined arithmetically in the body.}
\item{rng}{Passed to \code{.sp_correlogram}.}
\item{model}{Passed to \code{.sp_correlogram}.}
}
\description{
C(h) = sigma0^2 R(h) for h > 0, so the nugget enters only on the diagonal -- it is a discontinuity at the origin, not a term added everywhere.
}
\value{
The value of \code{sigma}, as built in the body.
}
\keyword{internal}
