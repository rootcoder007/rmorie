% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.baysmplr_leapfrog}
\alias{.baysmplr_leapfrog}
\title{.baysmplr_leapfrog}
\usage{
.baysmplr_leapfrog(grad, q, p, eps, steps)
}
\arguments{
\item{grad}{See Usage.}
\item{q}{Numeric; combined arithmetically in the body.}
\item{p}{Numeric; combined arithmetically in the body.}
\item{eps}{Numeric; combined arithmetically in the body.}
\item{steps}{Coerced to integer by the body, with \code{as.integer}.}
}
\description{
A step of the baysmplr_native implementation. Called by \code{.baysmplr_build_tree}, \code{morie_baysmplr_hmc}. See the file header for the source the module follows.
}
\value{
A list with \code{q}, \code{p}, \code{g}.
}
\references{
  Metropolis, N. et al. (1953) J. Chem. Phys. 21(6), 1087-1092.
  Hastings, W.K. (1970) Biometrika 57(1), 97-109.
  Geman, S. and Geman, D. (1984) IEEE PAMI 6(6), 721-741.
  Duane, S. et al. (1987) Physics Letters B 195(2), 216-222.
  Neal, R.M. (2011) "MCMC using Hamiltonian dynamics." Handbook of
    Markov Chain Monte Carlo, chapter 5.
  Hoffman, M.D. and Gelman, A. (2014) "The No-U-Turn Sampler." JMLR
    15, 1593-1623. Algorithms 3 and 6.
  Roberts, G.O., Gelman, A. and Gilks, W.R. (1997) Ann. Appl. Probab.
    7(1), 110-120.
  Geyer, C.J. (1992) Statist. Sci. 7(4), 473-483.
}
\keyword{internal}
