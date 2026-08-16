% Generated from the module source by ledger/gen_rd.R -- roxygen2 hangs
% on this package. Usage comes from args(), so it tracks the formals.
\name{morie_lcwphr_latent_class_weighted}
\alias{morie_lcwphr_latent_class_weighted}
\alias{morie_lcwphr}
\title{Latent class analysis by EM with inverse-probability-}
\usage{
morie_lcwphr_latent_class_weighted(y, A, H, K, trim = 0, stabilize = TRUE, max_iter = 500L, tol = 1e-11)
}
\description{
R arm of lcwphr -- latent class analysis by EM with inverse-probability- weighted class-specific treatment effects. 550-560.
}
\value{
A list with components \code{n}, \code{lab0}, \code{drop}, \code{estimate}, \code{class_ate}, \code{class_mean_treated}, \code{class_mean_control}, \code{naive_class_ate}, \code{naive_class_mean_treated}, \code{naive_class_mean_control}, \code{ate}, \code{naive_ate}, \code{marginal_ate}, \code{unweighted_ate}, \code{class_prevalence}, \code{item_probabilities}, \code{posterior}, \code{labels}, \code{propensity}, \code{propensity_coefficients}, \code{weights}, \code{effective_sample_size}, \code{weight_max}, \code{weight_mean}, \code{loglik}, \code{loglik_path}, \code{bic}, \code{entropy}, \code{n_parameters}, \code{iterations}, \code{converged}, \code{K}, \code{Q}, \code{stabilized}, \code{trim}, \code{method}, \code{note}.
}
\references{
Lanza, S. T., Coffman, D. L. & Xu, S. (2013) Structural Equation Modeling
20(3), 361-383; Goodman, L. A. (1974) Biometrika 61(2), 215-231;
Robins, J. M., Hernan, M. A. & Brumback, B. (2000) Epidemiology 11(5),
}
\keyword{internal}
