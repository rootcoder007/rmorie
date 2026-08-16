% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.prnFil_simulate_future_trend}
\alias{.prnFil_simulate_future_trend}
\title{.prnFil_simulate_future_trend}
\usage{
.prnFil_simulate_future_trend(fit, t_future, n_sims = 200, seed = 0)
}
\arguments{
\item{fit}{A list; the body reads \code{$changepoints}, \code{$deltas}, \code{$k}, \code{$m}, \code{$t} from it.}
\item{t_future}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{n_sims}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{200}.}
\item{seed}{Passed to \code{.ghc_rng}. Defaults to \code{0}.}
}
\description{
A step of the prnFil_native implementation. Called by \code{.prnFil_trend_intervals}. See the file header for the source the module follows.
}
\value{
The value of \code{sims}, as built in the body.
}
\references{
----------
Taylor, S. J. & Letham, B. (2018) "Forecasting at Scale", *The American
Statistician* 72(1), 37-45, doi:10.1080/00031305.2017.1380080;
preprint *PeerJ Preprints* 5:e3190v2,
doi:10.7287/peerj.preprints.3190v2. Sec. 3.1.3 (automatic changepoint
selection) and Sec. 3.1.4 (trend forecast uncertainty).

Tibshirani, R. (1996) "Regression Shrinkage and Selection via the
Lasso", *Journal of the Royal Statistical Society Series B* 58(1),
267-288, doi:10.1111/j.2517-6161.1996.tb02080.x. The L1 penalty the
Laplace prior corresponds to.

Park, T. & Casella, G. (2008) "The Bayesian Lasso", *Journal of the
American Statistical Association* 103(482), 681-686,
doi:10.1198/016214508000000337. The Laplace-prior formulation.
}
\keyword{internal}
