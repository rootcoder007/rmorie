% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_poissp}
\alias{morie_poissp}
\title{Fit the Poisson-CAR areal model.}
\usage{
morie_poissp(counts, X = NULL, offset = NULL, W = NULL, rho = 1, tau = NULL, constrain = NULL, iters = 100L, tol = 1e-11, ridge = 1e-10, tau_grid = NULL, level = 0.95)
}
\description{
Fit the Poisson-CAR areal model.
}
\value{
A list with \code{estimate}, \code{beta}, \code{se}, \code{lower}, \code{upper}, \code{u}, \code{eta}, \code{fitted}, \code{relative_risk}, \code{score_beta}, \code{loglik}, \code{deviance}, \code{tau}, \code{rho}, \code{spatial}, \code{constrained}, \code{n}, \code{p}, \code{level}, \code{method}, \code{note}.
}
\references{
Banerjee, S., Carlin, B. P. and Gelfand, A. E. (2014) Hierarchical
Modeling and Analysis for Spatial Data, 2nd edn, Monographs on
Statistics and Applied Probability 135, Chapman & Hall/CRC, Boca Raton,
ISBN 978-1-4398-1917-3 -- Ch. 4 (proper and intrinsic CAR) and Ch. 6
(Poisson-CAR disease mapping).
Besag, J., York, J. and Mollie, A. (1991) "Bayesian image restoration,
with two applications in spatial statistics", Annals of the Institute
of Statistical Mathematics 43(1), 1-20, doi:10.1007/BF00116466.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_poissp(V)
}
