% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_reinfc}
\alias{morie_reinfc}
\title{morie_reinfc}
\usage{
morie_reinfc(reward_fn, x = NULL, w = NULL, p = NULL, mu = 0, sigma = 1, unit = "bernoulli-logistic", baseline = "comparison", mode = "immediate", alpha = 0.1, gamma = 0.9, rho = 0.1, episode_length = 1, trials = 100, eligibility = "p", rate_scaling = "sigma2", seed = 0)
}
\arguments{
\item{reward_fn}{See Usage.}
\item{x}{Defaults to \code{NULL}.}
\item{w}{Defaults to \code{NULL}.}
\item{p}{Defaults to \code{NULL}.}
\item{mu}{Defaults to \code{0}.}
\item{sigma}{Defaults to \code{1}.}
\item{unit}{Defaults to \code{"bernoulli-logistic"}.}
\item{baseline}{Defaults to \code{"comparison"}.}
\item{mode}{Defaults to \code{"immediate"}.}
\item{alpha}{Defaults to \code{0.1}.}
\item{gamma}{Defaults to \code{0.9}.}
\item{rho}{Defaults to \code{0.1}.}
\item{episode_length}{Defaults to \code{1}.}
\item{trials}{Defaults to \code{100}.}
\item{eligibility}{Defaults to \code{"p"}.}
\item{rate_scaling}{Defaults to \code{"sigma2"}.}
\item{seed}{Defaults to \code{0}.}
}
\description{
Part of the reinfc_native implementation; see the file header for the source it follows.
}
\value{
The value of \code{.reinfc_run_logistic}.
}
\keyword{internal}
