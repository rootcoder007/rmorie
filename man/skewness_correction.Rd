% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{skewness_correction}
\alias{skewness_correction}
\title{skewness_correction}
\usage{
skewness_correction(third_derivative, precision) 
}
\arguments{
\item{third_derivative}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{precision}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the inlasm_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{skewness}, \code{gaussian_adequate}, \code{note}.
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
skewness_correction(third_derivative = c(1, 2, 3, 4, 5, 6, 7, 8), precision = 5L)
}
