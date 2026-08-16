% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_iv_liml_kappa}
\alias{.morie_iv_liml_kappa}
\title{Internal helper: LIML kappa via the eigenvalue problem}
\usage{
.morie_iv_liml_kappa(y, X_endo, Z_full, X_exo)
}
\description{
kappa = smallest eigenvalue of (W\' M_1 W)(W\' M_Z W)^{-1} where W = \[y, endogenous\], M_1 annihilates the exogenous block (incl. intercept) and M_Z annihilates the full instrument set.
}
\value{
A numeric value.
}
\keyword{internal}
