% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_evt_bayes_gev}
\alias{morie_evt_bayes_gev}
\title{Bayesian GEV posterior via random-walk Metropolis}
\usage{
morie_evt_bayes_gev(x, n_draws = 2000, seed = 42, prior_sd = c(100, 10, 1)) 
}
\arguments{
\item{x}{block maxima; n_draws retained draws; seed RNG seed}
\item{prior_sd}{prior standard deviations for (mu, log sigma, xi)}

\item{n_draws}{Argument \code{n_draws}; see Usage.}

\item{seed}{Argument \code{seed}; see Usage.}
}
\description{
(Coles 2001 sec. 9.1.3 vague-normal-prior recipe)
}
\value{
A list with \code{draws}, \code{accept_rate}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_evt_bayes_gev(V)
}
