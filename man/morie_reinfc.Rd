% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_reinfc}
\alias{morie_reinfc}
\title{morie_reinfc}
\usage{
morie_reinfc(reward_fn, x = NULL, w = NULL, p = NULL, mu = 0, sigma = 1, unit = "bernoulli-logistic", 
    baseline = "comparison", mode = "immediate", alpha = 0.1, gamma = 0.9, 
    rho = 0.1, episode_length = 1, trials = 100, eligibility = "p", rate_scaling = "sigma2", 
    seed = 0) 
}
\arguments{
\item{reward_fn}{Passed to \code{.reinfc_run_gaussian}.}
\item{x}{Optional; may be \code{NULL}. Passed to \code{.reinfc_as_matrix}.}
\item{w}{Optional; may be \code{NULL}. Passed to \code{.reinfc_as_matrix}.}
\item{p}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{mu}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0}.}
\item{sigma}{Passed to \code{.reinfc_run_gaussian}. Defaults to \code{1}.}
\item{unit}{One of \code{"bernoulli"}, \code{"gaussian"}. Defaults to \code{"bernoulli-logistic"}.}
\item{baseline}{Passed to \code{.reinfc_run_gaussian}. Defaults to \code{"comparison"}.}
\item{mode}{Compared against \code{"immediate"}. Defaults to \code{"immediate"}.}
\item{alpha}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.1}.}
\item{gamma}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.9}.}
\item{rho}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.1}.}
\item{episode_length}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{1}.}
\item{trials}{Passed to \code{.reinfc_run_gaussian}. Defaults to \code{100}.}
\item{eligibility}{One of \code{"p"}, \code{"ybar"}. Defaults to \code{"p"}.}
\item{rate_scaling}{Passed to \code{.reinfc_run_gaussian}. Defaults to \code{"sigma2"}.}
\item{seed}{Passed to \code{.ghc_rng}. Defaults to \code{0}.}
}
\description{
A step of the reinfc_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{.reinfc_run_logistic}.
}
\keyword{internal}
