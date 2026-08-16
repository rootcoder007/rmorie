% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_rrblpr_rr_blup}
\alias{morie_rrblpr_rr_blup}
\title{morie_rrblpr_rr_blup}
\usage{
morie_rrblpr_rr_blup(y, M, lam = NULL, X = NULL, M_new = NULL, log_lam_lo = -12, log_lam_hi = 12, max_iter = 200L, tol = 1e-09)
}
\arguments{
\item{y}{See Usage.}
\item{M}{See Usage.}
\item{lam}{Defaults to \code{NULL}.}
\item{X}{Defaults to \code{NULL}.}
\item{M_new}{Defaults to \code{NULL}.}
\item{log_lam_lo}{Defaults to \code{-12}.}
\item{log_lam_hi}{Defaults to \code{12}.}
\item{max_iter}{Defaults to \code{200L}.}
\item{tol}{Defaults to \code{1e-09}.}
}
\description{
Part of the rrblpr_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{marker_effects}, \code{coefficients}, \code{breeding_values}, \code{breeding_values_kernel}, \code{kernel_identity_gap}, \code{fitted}, \code{residuals}, \code{lambda}, \code{lambda_estimated}, \code{sigma2_e}, \code{sigma2_u}, \code{sigma2_g}, \code{h2}, \code{reml_loglik}, \code{reml_profile}, \code{prediction_new}, \code{n}, \code{m}, \code{p}, \code{method}, \code{note}.
}
\keyword{internal}
