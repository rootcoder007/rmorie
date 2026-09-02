% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_rrblpr_rr_blup}
\alias{morie_rrblpr_rr_blup}
\title{morie_rrblpr_rr_blup}
\usage{
morie_rrblpr_rr_blup(y, M, lam = NULL, X = NULL, M_new = NULL, log_lam_lo = -12, log_lam_hi = 12, 
    max_iter = 200L, tol = 1e-09) 
}
\arguments{
\item{y}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{M}{Passed to \code{.rrblpr_rows}.}
\item{lam}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{X}{Optional; may be \code{NULL}. Passed to \code{.rrblpr_rows}.}
\item{M_new}{Optional; may be \code{NULL}. Passed to \code{.rrblpr_rows}.}
\item{log_lam_lo}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{-12}.}
\item{log_lam_hi}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{12}.}
\item{max_iter}{Accepted by the signature and not used anywhere in the body. Defaults to \code{200L}.}
\item{tol}{Accepted by the signature and not used anywhere in the body. Defaults to \code{1e-09}.}
}
\description{
A step of the rrblpr_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{marker_effects}, \code{coefficients}, \code{breeding_values}, \code{breeding_values_kernel}, \code{kernel_identity_gap}, \code{fitted}, \code{residuals}, \code{lambda}, \code{lambda_estimated}, \code{sigma2_e}, \code{sigma2_u}, \code{sigma2_g}, \code{h2}, \code{reml_loglik}, \code{reml_profile}, \code{prediction_new}, \code{n}, \code{m}, \code{p}, \code{method}, \code{note}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_rrblpr_rr_blup(V, V)
}
