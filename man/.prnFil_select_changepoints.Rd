% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.prnFil_select_changepoints}
\alias{.prnFil_select_changepoints}
\title{.prnFil_select_changepoints}
\usage{
.prnFil_select_changepoints(t, y, tau = 0.05, n_changepoints = 15, changepoint_range = 0.8, seasonalities = NULL, ...)
}
\arguments{
\item{t}{See Usage.}
\item{y}{See Usage.}
\item{tau}{Defaults to \code{0.05}.}
\item{n_changepoints}{Defaults to \code{15}.}
\item{changepoint_range}{Defaults to \code{0.8}.}
\item{seasonalities}{Defaults to \code{NULL}.}
\item{...}{Passed through.}
}
\description{
Part of the prnFil_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{selected}, \code{selected_index}, \code{deltas}, \code{candidates}, \code{n_selected}, \code{n_candidates}, \code{tau}, \code{fit}, \code{last_candidate_fraction}, \code{changepoint_range}, \code{rmse}, \code{method}.
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
