% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{BwDigital}
\alias{BwDigital}
\title{BwDigital}
\usage{
BwDigital(Omega_c = NULL, N = NULL, T = 1, fc = NULL, fs = NULL, z = NULL)
}
\arguments{
\item{Omega_c}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{N}{Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.}
\item{T}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1}.}
\item{fc}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{fs}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{z}{Optional; may be \code{NULL}. Passed to \code{.morie_rg_polyz}.}
}
\description{
A step of the rangayyan_filt2 implementation. Called by \code{BwHp}, \code{BwLp}. See the file header for the source the module follows.
}
\value{
A list with \code{b}, \code{a}, \code{gain}, \code{poles_z}, \code{H}, \code{N}, \code{Omega_c}, \code{T}, \code{prewarped_here}, \code{zeros_at_minus_one}, \code{zeros_are_forced_by_the_bilinear_transform}, \code{dc_gain}, \code{leading_a_is_one}, \code{method}.
}
\keyword{internal}
\examples{
BwDigital(N = 4, fc = 100, fs = 1000)
}
