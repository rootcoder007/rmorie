% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_slbpdg_weights}
\alias{morie_slbpdg_weights}
\title{Stick-breaking weights from the beta variates}
\usage{
morie_slbpdg_weights(v)
}
\arguments{
\item{v}{The beta variates.}
}
\description{
w_k = v_k times the product of (1 - v_l) over l < k, accumulated as a running product rather than recomputed -- both the cheap way and the way the Python arm can be matched term for term.
}
\value{
A list with the weights and the unbroken remainder.
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
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_slbpdg_weights(V)
}
