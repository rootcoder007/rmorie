% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{laplace_marginal}
\alias{laplace_marginal}
\title{laplace_marginal}
\usage{
laplace_marginal(log_joint, x_grid, theta)
}
\arguments{
\item{log_joint}{Accepted by the signature and not used anywhere in the body.}
\item{x_grid}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{theta}{Passed to \code{log_joint}.}
}
\description{
A step of the inlasm_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{x}, \code{density}, \code{mean}, \code{sd}, \code{log_scale}.
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
\examples{
lm <- laplace_marginal(
  log_joint = function(x, theta) -0.5 * (x - theta)^2,
  x_grid = seq(-4, 6, by = 0.05), theta = 2)
abs(lm$mean - 2) < 0.01
}
