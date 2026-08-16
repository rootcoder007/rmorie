% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_baysmplr_ess}
\alias{morie_baysmplr_ess}
\title{Geyer's initial positive sequence estimator, per coordinate}
\usage{
morie_baysmplr_ess(chain, max_lag = 200L)
}
\arguments{
\item{chain}{A list of draws.}
\item{max_lag}{Largest lag computed.}
}
\description{
Sums the autocorrelations in adjacent PAIRS and stops when a pair turns negative. The pairing is not decoration: for a reversible chain the sum of two adjacent autocorrelations is positive, so truncating on a single negative lag stops too early on a noisy tail and inflates the answer. Lags beyond max_lag are not computed. The estimator is meant to stop long before that -- if it has not, the chain has an autocorrelation time comparable to its own length and the number it would return is not trustworthy anyway.
}
\value{
One effective sample size per coordinate.
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
