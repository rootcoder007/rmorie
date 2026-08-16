% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_rkhs_cov}
\alias{morie_rkhs_cov}
\title{morie_rkhs_cov}
\usage{
morie_rkhs_cov(Z_L, G, Z_LE = NULL, I_env = NULL, sigma2_g = 1, sigma2_ge = 1)
}
\description{
morie_rkhs_cov
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
n <- 12
A <- matrix(rnorm(n * 6), nrow = n)
G <- morie_grm(A) + diag(0.3, n)
morie_chol_lower(G)
morie_rkhs_cov(Z_L = G, G = G)
}
