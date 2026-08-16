% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.slbpdg_theta}
\alias{.slbpdg_theta}
\title{Conjugate normal / inverse-gamma draw for one component. With no}
\usage{
.slbpdg_theta(e, ys, m0, kappa0, a0, b0)
}
\arguments{
\item{e}{Passed to \code{.ghc_gamma1}.}
\item{ys}{A vector; its length is taken.}
\item{m0}{Numeric; combined arithmetically in the body.}
\item{kappa0}{Numeric; combined arithmetically in the body.}
\item{a0}{Numeric; combined arithmetically in the body.}
\item{b0}{Numeric; combined arithmetically in the body.}
}
\description{
members this is a draw from the PRIOR, which is what the slice sampler needs for components it has to invent to cover the slice -- they are not fitted to anything, so their parameters must come from the prior and not from some neighbour.
}
\value{
A vector, from \code{c}.
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
