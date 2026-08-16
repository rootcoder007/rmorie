% Generated from the module source by ledger/gen_rd.R -- roxygen2 hangs
% on this package. Usage comes from args(), so it tracks the formals.
\name{morie_rrblpr_rr_blup}
\alias{morie_rrblpr_rr_blup}
\alias{morie_rrblpr}
\title{Ridge-regression BLUP through Henderson's mixed model}
\usage{
morie_rrblpr_rr_blup(y, M, lam = NULL, X = NULL, M_new = NULL, log_lam_lo = -12, log_lam_hi = 12, max_iter = 200L, tol = 1e-09)
}
\description{
R arm of rrblpr -- ridge-regression BLUP through Henderson's mixed model equations, the variance ratio estimated by profile REML when not supplied.
}
\value{
A list with components \code{n}, \code{lambda}, \code{estimate}, \code{marker_effects}, \code{coefficients}, \code{breeding_values}, \code{breeding_values_kernel}, \code{kernel_identity_gap}, \code{fitted}, \code{residuals}, \code{lambda_estimated}, \code{sigma2_e}, \code{sigma2_u}, \code{sigma2_g}, \code{h2}, \code{reml_loglik}, \code{reml_profile}, \code{prediction_new}, \code{m}, \code{p}, \code{method}, \code{note}.
}
\references{
Whittaker, J. C., Thompson, R. & Denham, M. C. (2000) Genetical Research
75(2), 249-252; Meuwissen, T. H. E. et al. (2001) Genetics 157(4),
1819-1829; Henderson, C. R. (1975) Biometrics 31(2), 423-447.
}
\keyword{internal}
