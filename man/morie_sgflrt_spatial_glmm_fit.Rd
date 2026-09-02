% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_sgflrt_spatial_glmm_fit}
\alias{morie_sgflrt_spatial_glmm_fit}
\title{morie_sgflrt_spatial_glmm_fit}
\usage{
morie_sgflrt_spatial_glmm_fit(y, X, coords, family = "poisson", model = "exponential", sigma2 = NULL, 
    phi = NULL, kappa = 1.5, nugget = 0, dispersion = NULL, inner_iter = 50L, 
    outer_cycles = 3L, tol = 1e-10) 
}
\arguments{
\item{y}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{X}{Passed to \code{.sgflrt_rows}.}
\item{coords}{Passed to \code{.sgflrt_rows}.}
\item{family}{One of \code{"binomial"}, \code{"gaussian"}, \code{"poisson"}. Defaults to \code{"poisson"}.}
\item{model}{Passed to \code{.sgflrt_corr}. Defaults to \code{"exponential"}.}
\item{sigma2}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{phi}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{kappa}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1.5}.}
\item{nugget}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0}.}
\item{dispersion}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{inner_iter}{Passed to \code{.sgflrt_laplace}. Defaults to \code{50L}.}
\item{outer_cycles}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{3L}.}
\item{tol}{Passed to \code{.sgflrt_laplace}. Defaults to \code{1e-10}.}
}
\description{
A step of the sgflrt_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{coefficients}, \code{std_error}, \code{z}, \code{spatial_effect}, \code{fitted}, \code{linear_predictor}, \code{sigma2}, \code{phi}, \code{dispersion}, \code{sigma2_at_lower_bound}, \code{spatial_signal}, \code{kappa}, \code{nugget}, \code{loglik}, \code{laplace_loglik}, \code{gls_identity_gap}, \code{covariance}, \code{family}, \code{model}, \code{n}, \code{p}, \code{d}, \code{min_distance}, \code{max_distance}, \code{method}, \code{note}.
}
\keyword{internal}
\examples{
morie_sgflrt_spatial_glmm_fit(y = c(1, 2, 3, 4, 5, 6, 7, 8), X = c(1, 2, 3, 4, 5, 6, 7, 8), coords = c(1, 2, 3, 4, 5, 6, 7, 8))
}
