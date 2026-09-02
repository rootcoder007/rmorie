% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_baysmplr_gibbs}
\alias{morie_baysmplr_gibbs}
\title{Gibbs on a multivariate normal, using its exact conditionals}
\usage{
morie_baysmplr_gibbs(mean, cov_inv, x0, n_iter, e) 
}
\arguments{
\item{mean}{The mean vector.}
\item{cov_inv}{The precision matrix.}
\item{x0}{Starting point.}
\item{n_iter}{Iterations.}
\item{e}{A generator environment from .ghc_rng.}
}
\description{
The conditional of coordinate j given the rest is normal with precision Q_jj and mean mu_j minus the weighted deviations of the others. Written from the precision matrix because that is the form in which the conditionals are trivial -- inverting a covariance to get them and then inverting back is the usual way to make this slower and less accurate than it needs to be.
}
\value{
A list with the draws, the acceptance rate and the settings.
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
\examples{
MU3 <- c(1, -0.5, 2)
Q3 <- matrix(c(2, 0.6, 0.1, 0.6, 1.5, -0.3, 0.1, -0.3, 1.2), 
    3, 3, byrow = TRUE)
e <- .ghc_rng(13)
e <- .ghc_rng(13)
e <- .ghc_rng(13)
e <- .ghc_rng(13)
e <- .ghc_rng(13)
e <- .ghc_rng(13)
morie_baysmplr_gibbs(MU3, Q3, c(0, 0, 0), 60L, e)
}
