% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_funBand_influence_matrix}
\alias{morie_funBand_influence_matrix}
\title{The influence matrix A(lambda) = (I + lambda K)^-1, computed spectrally.}
\usage{
morie_funBand_influence_matrix(x, lam)
}
\description{
K = U diag(d) U\', so A = U diag(1/(1 + lambda d)) U\'. Factorising I + lambda K directly loses positive definiteness once lambda * ||K|| reaches about 1e12, which is exactly the limit the anchors probe.
}
\value{
A numeric value.
}
\references{
Wahba, G. (1983) "Bayesian \'confidence intervals\' for the cross-validated
smoothing spline", Journal of the Royal Statistical Society Series B
(Methodological) 45(1), 133-150, doi:10.1111/j.2517-6161.1983.tb01239.x.
Craven, P. and Wahba, G. (1979) "Smoothing noisy data with spline functions",
Numerische Mathematik 31(4), 377-403, doi:10.1007/BF01404567.
Green, P. J. and Silverman, B. W. (1994) Nonparametric Regression and
Generalized Linear Models, Chapman and Hall, ISBN 978-0-412-30040-0, Sec 2.1.2.
}
\keyword{internal}
\examples{
morie_funBand_influence_matrix(x = c(1, 2, 3, 4, 5, 6, 7, 8), lam = 5L)
}
