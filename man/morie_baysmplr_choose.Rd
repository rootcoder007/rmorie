% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_baysmplr_choose}
\alias{morie_baysmplr_choose}
\title{The dispatch decision and the sentence that explains it}
\usage{
morie_baysmplr_choose(dim, has_grad, has_conditionals = FALSE, nuts_threshold = 20L)
}
\arguments{
\item{dim}{Dimension of the parameter.}
\item{has_grad}{Whether a gradient is available.}
\item{has_conditionals}{Whether exact conditionals are available.}
\item{nuts_threshold}{Dimension at or above which NUTS is preferred.}
}
\description{
The dispatch decision and the sentence that explains it
}
\value{
A list with the sampler name and the reason.
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
