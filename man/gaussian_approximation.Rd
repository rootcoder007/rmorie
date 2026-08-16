% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{gaussian_approximation}
\alias{gaussian_approximation}
\title{gaussian_approximation}
\usage{
gaussian_approximation(log_lik, log_lik_d1, log_lik_d2, prior_mean, prior_precision, x0 = 0, iters = 60, tol = 1e-12)
}
\arguments{
\item{log_lik}{See Usage.}
\item{log_lik_d1}{See Usage.}
\item{log_lik_d2}{See Usage.}
\item{prior_mean}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{prior_precision}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{x0}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0}.}
\item{iters}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{60}.}
\item{tol}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1e-12}.}
}
\description{
A step of the inlasm_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{mode}, \code{precision}, \code{sd}, \code{iterations}, \code{log_norm}, \code{note}.
}
\references{
----------
Rue, H., Martino, S. & Chopin, N. (2009) "Approximate Bayesian
inference for latent Gaussian models by using integrated nested
Laplace approximations", Journal of the Royal Statistical Society:
Series B (Statistical Methodology) 71(2), 319-392,
doi:10.1111/j.1467-9868.2008.00700.x. [PDF supplied by Vee.]
Tierney, L. & Kadane, J. B. (1986) "Accurate Approximations for
Posterior Moments and Marginal Densities", Journal of the American
Statistical Association 81(393), 82-86,
doi:10.1080/01621459.1986.10478240. The Laplace approximation
being nested.
Rue, H. & Held, L. (2005) Gaussian Markov Random Fields: Theory
and Applications, Chapman & Hall/CRC,
doi:10.1201/9780203492024. The sparse GMRF computations the
method rests on.
}
\keyword{internal}
