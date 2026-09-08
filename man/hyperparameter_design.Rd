% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{hyperparameter_design}
\alias{hyperparameter_design}
\title{hyperparameter_design}
\usage{
hyperparameter_design(mode, curvature, step = 1, dim = NULL) 
}
\arguments{
\item{mode}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{curvature}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{step}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1}.}
\item{dim}{Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.}
}
\description{
A step of the inlasm_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{points}, \code{n_points}, \code{dim}, \code{cost_scaling}, \code{note}.
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
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
hyperparameter_design(M, V)
}
