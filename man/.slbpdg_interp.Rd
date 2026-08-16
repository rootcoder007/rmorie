% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.slbpdg_interp}
\alias{.slbpdg_interp}
\title{Linear interpolation, used only to integrate the density grid}
\usage{
.slbpdg_interp(xs, ys, x)
}
\arguments{
\item{xs}{A vector; its length is taken and its elements indexed.}
\item{ys}{A vector; indexed elementwise.}
\item{x}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the slbpdg_native implementation. Called by \code{morie_slbpdg}. See the file header for the source the module follows.
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
