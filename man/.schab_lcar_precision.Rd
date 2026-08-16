% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.schab_lcar_precision}
\alias{.schab_lcar_precision}
\title{.schab_lcar_precision}
\usage{
.schab_lcar_precision(R, rho, sigma2 = 1)
}
\arguments{
\item{R}{A matrix; passed to \code{nrow}.}
\item{rho}{Numeric; combined arithmetically in the body.}
\item{sigma2}{The body requires: `sigma2` must be positive. Defaults to \code{1}.}
}
\description{
A step of the schab_glmm_shared implementation. Called by \code{spbayr}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
.schab_lcar_precision(R = 5L, rho = 0.5)
}
