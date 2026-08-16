% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.schab_icar_covariance}
\alias{.schab_icar_covariance}
\title{Sigma^2 R^-, the Moore-Penrose inverse: R is singular by construction}
\usage{
.schab_icar_covariance(R, sigma2 = 1)
}
\arguments{
\item{R}{A matrix; passed to \code{as.matrix}.}
\item{sigma2}{Defaults to \code{1}.}
}
\description{
A step of the schab_glmm_shared implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
