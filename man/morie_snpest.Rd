% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_snpest}
\alias{morie_snpest}
\title{On-line DP-mixture filter over a stream of observations}
\usage{
morie_snpest(y_stream, alpha = 1, n_particles = 100L, proposal = "optimal", resampler = "systematic", ess_threshold = 0.5, m0 = NULL, kappa0 = 0.01, a0 = 2, b0 = NULL, seed = 1, grid = NULL, seed_stats = TRUE)
}
\arguments{
\item{y_stream}{The observations, in arrival order. The order MATTERS:   this is a filter, not a batch method, and the answer after t points   is a function of the first t.}
\item{alpha}{Dirichlet-process concentration.}
\item{n_particles}{Particles carried.}
\item{proposal}{"optimal" or "prior".}
\item{resampler}{"systematic", "stratified" or "multinomial".}
\item{ess_threshold}{Resample when the effective sample size falls   below this fraction of the particle count.}
\item{m0}{Prior mean; the stream mean by default.}
\item{kappa0}{Prior precision scaling on the mean.}
\item{a0}{Inverse-gamma shape.}
\item{b0}{Inverse-gamma scale; from the stream variance by default.}
\item{seed}{Seed for the generator shared with the Python arm.}
\item{grid}{Points at which the filtered predictive density is   recorded after the last observation.}
\item{seed_stats}{Take m0 and b0 from the whole stream when not given.   FALSE uses 0 and 1 instead and keeps the run strictly causal.}
}
\description{
On-line DP-mixture filter over a stream of observations
}
\value{
A list with the running log marginal likelihood, the expected   number of clusters after each point, the ESS trace, the resampling   times, and the filtered predictive density on the grid.
}
\references{
  MacEachern, S.N., Clyde, M. and Liu, J.S. (1999) "Sequential
    importance sampling for nonparametric Bayes models: the next
    generation." Canadian Journal of Statistics 27(2), 251-267.
  Fearnhead, P. (2004) "Particle filters for mixture models with an
    unknown number of components." Statistics and Computing 14(1),
    11-21.
  Caron, F., Doucet, A. and Gottardo, R. (2012) "On-line changepoint
    detection and parameter estimation with application to genomic
    data." Statistics and Computing 22(2), 579-595. The ledger cites
    this as 2017; the paper is 2012, and the year is corrected here
    rather than repeated.
  Escobar, M.D. and West, M. (1995) JASA 90(430), 577-588.
}
\keyword{internal}
