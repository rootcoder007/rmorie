% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_baysmplr_nuts}
\alias{morie_baysmplr_nuts}
\title{The No-U-Turn Sampler, with optional dual-averaging warm-up}
\usage{
morie_baysmplr_nuts(log_p, grad, x0, n_iter, e, eps = 0.25, max_depth = 8L, dual_average = FALSE, 
    target_accept = 0.8, warmup = NULL) 
}
\arguments{
\item{log_p}{The log density.}
\item{grad}{Its gradient.}
\item{x0}{Starting point.}
\item{n_iter}{Iterations.}
\item{e}{A generator environment from .ghc_rng.}
\item{eps}{Step size.}
\item{max_depth}{Doubling limit.}
\item{dual_average}{Tune the step size during warm-up.}
\item{target_accept}{Target acceptance statistic.}
\item{warmup}{Warm-up iterations; half of n_iter by default.}
}
\description{
The trajectory doubles until the two ends start approaching each other, which is what removes the trajectory-length parameter. With dual_average the step size is tuned during warm-up by Hoffman and Gelman's Algorithm 6 and then frozen, so the retained chain has a single fixed step size rather than a moving one.
}
\value{
A list with the draws, the mean acceptance statistic and the   settings.
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
gr3 <- function(x) {
    dv <- x - MU3
    vapply(1:3, function(i) -.w3_csum(vapply(1:3, function(j) Q3[i, 
        j] * dv[j], numeric(1))), numeric(1))
}
e <- rmorie:::.ghc_rng(13)
e <- rmorie:::.ghc_rng(13)
e <- rmorie:::.ghc_rng(13)
e <- rmorie:::.ghc_rng(13)
e <- rmorie:::.ghc_rng(13)
e <- rmorie:::.ghc_rng(13)
morie_baysmplr_nuts(lp3, gr3, c(0, 0, 0), 40L, e, 0.3, 6L)
}
