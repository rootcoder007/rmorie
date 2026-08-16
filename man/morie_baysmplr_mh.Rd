% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_baysmplr_mh}
\alias{morie_baysmplr_mh}
\title{Random-walk Metropolis with an isotropic normal proposal}
\usage{
morie_baysmplr_mh(log_p, x0, n_iter, e, scale = NULL, adapt = FALSE, target_accept = 0.234)
}
\arguments{
\item{log_p}{The log density.}
\item{x0}{Starting point.}
\item{n_iter}{Iterations.}
\item{e}{A generator environment from .ghc_rng.}
\item{scale}{Proposal scale, or NULL for the default.}
\item{adapt}{Tune the scale during the first half.}
\item{target_accept}{Target acceptance rate.}
}
\description{
The default scale is 2.38 / sqrt(d), the optimal-scaling value for a product target; adapt then tunes it towards target_accept by Robbins-Monro on the log scale during the first half of the run, after which it is frozen so the chain that is kept is homogeneous.
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
lp3 <- function(x) {
    dv <- x - MU3
    terms <- numeric(9)
    k <- 1L
    for (i in 1:3) for (j in 1:3) {
        terms[k] <- dv[i] * Q3[i, j] * dv[j]
        k <- k + 1L
    }
    -0.5 * .w3_csum(terms)
}
e <- .ghc_rng(13)
e <- .ghc_rng(13)
e <- .ghc_rng(13)
e <- .ghc_rng(13)
e <- .ghc_rng(13)
e <- .ghc_rng(13)
morie_baysmplr_mh(lp3, c(0, 0, 0), 60L, e)
}
