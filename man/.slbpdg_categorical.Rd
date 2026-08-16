% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.slbpdg_categorical}
\alias{.slbpdg_categorical}
\title{Inverse-CDF draw on UNNORMALISED weights, one uniform each. The}
\usage{
.slbpdg_categorical(e, weights)
}
\arguments{
\item{e}{Passed to \code{.ghc_unif}.}
\item{weights}{A vector; its length is taken and its elements indexed.}
}
\description{
cumulative sum is a plain running sum in both arms rather than a compensated one, because the comparison is against a uniform scaled by the same total -- consistency between the two sums is what matters, not their accuracy.
}
\value{
A numeric value.
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
