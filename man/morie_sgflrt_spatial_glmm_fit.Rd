% Generated from the module source by ledger/gen_rd.R -- roxygen2 hangs
% on this package. Usage comes from args(), so it tracks the formals.
\name{morie_sgflrt_spatial_glmm_fit}
\alias{morie_sgflrt_spatial_glmm_fit}
\alias{morie_sgflrt}
\title{Spatial GLMM by the Laplace approximation, with the}
\usage{
morie_sgflrt_spatial_glmm_fit(y, X, coords, family = "poisson", model = "exponential", sigma2 = NULL, phi = NULL, kappa = 1.5, nugget = 0, dispersion = NULL, inner_iter = 50L, outer_cycles = 3L, tol = 1e-10)
}
\description{
R arm of sgflrt -- spatial GLMM by the Laplace approximation, with the random effect carried as u = Lv so Sigma^-1 is never formed.
}
\value{
A list with components \code{n}, \code{family}, \code{nrow}, \code{estimate}, \code{coefficients}, \code{std_error}, \code{z}, \code{spatial_effect}, \code{fitted}, \code{linear_predictor}, \code{sigma2}, \code{phi}, \code{dispersion}, \code{sigma2_at_lower_bound}, \code{spatial_signal}, \code{kappa}, \code{nugget}, \code{loglik}, \code{laplace_loglik}, \code{gls_identity_gap}, \code{covariance}, \code{model}, \code{p}, \code{d}, \code{min_distance}, \code{max_distance}, \code{method}, \code{note}.
}
\references{
Diggle, P. J., Tawn, J. A. & Moyeed, R. A. (1998) JRSS-C 47(3), 299-350;
Breslow, N. E. & Clayton, D. G. (1993) JASA 88(421), 9-25;
Tierney, L. & Kadane, J. B. (1986) JASA 81(393), 82-86.
}
\keyword{internal}
