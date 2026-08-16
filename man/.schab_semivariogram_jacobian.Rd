% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.schab_semivariogram_jacobian}
\alias{.schab_semivariogram_jacobian}
\title{D gamma(h; c0, sigma0^2, a) / d(c0, sigma0^2, a), one row per lag}
\usage{
.schab_semivariogram_jacobian(h, nugget, sill, rng, model)
}
\arguments{
\item{h}{A vector; its length is taken and its elements indexed.}
\item{nugget}{Accepted by the signature and not used anywhere in the body.}
\item{sill}{Numeric; combined arithmetically in the body.}
\item{rng}{Numeric; combined arithmetically in the body.}
\item{model}{Passed to \code{.sp_correlogram}.}
}
\description{
d gamma / d c0 = 1 d gamma / d sigma0^2 = 1 - R(h; a) d gamma / d a = -sigma0^2 dR/da Every row is zero at h = 0, where gamma(0) = 0 by definition whatever the nugget (Sec. 4.3.6).
}
\value{
The value of \code{jac}, as built in the body.
}
\keyword{internal}
