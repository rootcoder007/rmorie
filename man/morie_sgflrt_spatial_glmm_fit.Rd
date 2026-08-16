% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_sgflrt_spatial_glmm_fit}
\alias{morie_sgflrt_spatial_glmm_fit}
\title{morie_sgflrt_spatial_glmm_fit}
\usage{
morie_sgflrt_spatial_glmm_fit(y, X, coords, family = "poisson", model = "exponential", sigma2 = NULL, phi = NULL, kappa = 1.5, nugget = 0, dispersion = NULL, inner_iter = 50L, outer_cycles = 3L, tol = 1e-10)
}
\arguments{
\item{y}{See Usage.}
\item{X}{See Usage.}
\item{coords}{See Usage.}
\item{family}{Defaults to \code{"poisson"}.}
\item{model}{Defaults to \code{"exponential"}.}
\item{sigma2}{Defaults to \code{NULL}.}
\item{phi}{Defaults to \code{NULL}.}
\item{kappa}{Defaults to \code{1.5}.}
\item{nugget}{Defaults to \code{0}.}
\item{dispersion}{Defaults to \code{NULL}.}
\item{inner_iter}{Defaults to \code{50L}.}
\item{outer_cycles}{Defaults to \code{3L}.}
\item{tol}{Defaults to \code{1e-10}.}
}
\description{
Part of the sgflrt_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{coefficients}, \code{std_error}, \code{z}, \code{spatial_effect}, \code{fitted}, \code{linear_predictor}, \code{sigma2}, \code{phi}, \code{dispersion}, \code{sigma2_at_lower_bound}, \code{spatial_signal}, \code{kappa}, \code{nugget}, \code{loglik}, \code{laplace_loglik}, \code{gls_identity_gap}, \code{covariance}, \code{family}, \code{model}, \code{n}, \code{p}, \code{d}, \code{min_distance}, \code{max_distance}, \code{method}, \code{note}.
}
\keyword{internal}
