% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_slbpdg_density}
\alias{morie_slbpdg_density}
\title{Mixture density at a point}
\usage{
morie_slbpdg_density(x, w, mu, s2)
}
\arguments{
\item{x}{The point.}
\item{w}{Component weights.}
\item{mu}{Component means.}
\item{s2}{Component variances.}
}
\description{
Mixture density at a point
}
\value{
The density value.
}
\references{
  Walker, S.G. (2007) "Sampling the Dirichlet mixture model with
    slices." Communications in Statistics -- Simulation and
    Computation 36(1), 45-54.
  Kalli, M., Griffin, J.E. and Walker, S.G. (2011) "Slice sampling
    mixture models." Statistics and Computing 21(1), 93-105.
  Escobar, M.D. and West, M. (1995) "Bayesian density estimation and
    inference using mixtures." JASA 90(430), 577-588, section 6.
  Ishwaran, H. and James, L.F. (2001) "Gibbs sampling methods for
    stick-breaking priors." JASA 96(453), 161-173.
}
\keyword{internal}
\examples{
V <- c(0.3, 0.5, 0.2, 0.8, 0.1)
sw <- morie_slbpdg_weights(V)
w <- sw$w
x <- -12
morie_slbpdg_density(x, w, c(-2, 0, 1, 3, 5), c(0.5, 1, 0.25, 
    2, 0.75))
}
