% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.sp_ordinary_kriging}
\alias{.sp_ordinary_kriging}
\title{Internal: ordinary kriging in terms of the semivariogram, Sec 5.2.2.2.}
\usage{
.sp_ordinary_kriging(coords, z, target, semivariogram_fn)
}
\description{
eqs (5.19)-(5.22) with Gamma = [gamma(s_i - s_j)] and gamma(s0) = [gamma(s0 - s_1), ..., gamma(s0 - s_n)]\'. The SIGN of the Lagrange multiplier matters downstream: the trans-Gaussian correction (5.58) uses (sigma^2_ok - 2 m), so a flipped m moves every prediction with no other symptom. (5.22) gives two expressions for the variance, which the suite asserts against each other.
}
\value{
A list with \code{prediction}, \code{variance}, \code{weights}, \code{lagrange}.
}
\keyword{internal}
