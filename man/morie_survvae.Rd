% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_survvae}
\alias{morie_survvae}
\title{Maximise the combined loss over gates and expert parameters}
\usage{
morie_survvae(X, times, events, K = 3L, primitive = "weibull", alpha = 1, prior = 0, 
    seed = 0, restarts = 4L) 
}
\arguments{
\item{X}{List of covariate vectors, length \code{n}.}
\item{times}{Numeric vector of survival times, length \code{n}.}
\item{events}{Event indicator (0/1 or logical), length \code{n}.}
\item{K}{Number of experts.}
\item{primitive}{One of \code{"weibull"} or \code{"lognormal"}.}
\item{alpha}{Discount on the censored term, in \code{[0, 1]}.}
\item{prior}{Strength of the log-parameter L2 penalty.}
\item{seed}{Seed for the shared generator (see   \code{.ghc_rng}).}
\item{restarts}{Number of random restarts; values below 1 are   treated as 1.}
}
\description{
Nelder-Mead over the stacked vector \eqn{(W, b, \log \beta, \log \eta)} with random multi-restart. Each restart builds its own starting point from \code{t0} (the mean of the positive observed times) and the shared RNG, then calls Nelder-Mead up to six times -- if a call lowers the objective by more than \code{1e-9} the new point becomes the start of the next call, exactly mirroring the Python arm\'s \code{"for _ in range(6)"} loop in \code{fit()}.
}
\value{
Named list with element names matching the Python   payload keys: \code{estimate}, \code{elbo}, \code{loglik},   \code{jensen_gap}, \code{W}, \code{bias}, \code{shapes},   \code{scales}, \code{K}, \code{primitive}, \code{alpha},   \code{prior}, \code{times}, \code{events}, \code{method}.
}
\keyword{internal}
\examples{
morie_survvae(X = c(1, 2, 3, 4, 5, 6, 7, 8), times = c(1, 2, 3, 4, 5, 6, 7, 8), events = c(1, 2, 3, 4, 5, 6, 7, 8))
}
