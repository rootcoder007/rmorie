% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.baysmplr_build_tree}
\alias{.baysmplr_build_tree}
\title{Hoffman and Gelman\\\'s recursive doubling, Algorithm 3}
\usage{
.baysmplr_build_tree(log_p, grad, q, p, u, v, j, eps, e, h0, dmax = 1000)
}
\arguments{
\item{log_p}{See Usage.}
\item{grad}{See Usage.}
\item{q}{See Usage.}
\item{p}{See Usage.}
\item{u}{See Usage.}
\item{v}{See Usage.}
\item{j}{See Usage.}
\item{eps}{See Usage.}
\item{e}{See Usage.}
\item{h0}{See Usage.}
\item{dmax}{Defaults to \code{1000}.}
}
\description{
Part of the baysmplr_native implementation; see the file header for the source it follows.
}
\value{
The value of \code{r}, as built in the body.
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
