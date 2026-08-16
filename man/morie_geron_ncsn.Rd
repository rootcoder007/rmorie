% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_geron_ncsn}
\alias{morie_geron_ncsn}
\title{Noise conditional score network: affine score models per noise level (Geron Ch 18, hmncsn)}
\usage{
morie_geron_ncsn(X, sigmas = 1, epochs = 400, lr = 0.5, n_noise = 32, seed = 0, n_samples = 0, langevin_steps = 20, step_eps = 0.05)
}
\arguments{
\item{X}{Training data (m, d).}
\item{sigmas}{Noise ladder.}
\item{epochs,lr,n_noise,seed,n_samples,langevin_steps,step_eps}{Training/sampling controls.}
}
\description{
Noise conditional score network: affine score models per noise level (Geron Ch 18, hmncsn)
}
\value{
A list with \code{models}, \code{analytic}, \code{max_deviation}, \code{loss_history}, \code{samples}, \code{sigmas}, \code{mean}, \code{covariance}, \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_geron_ncsn(V)
}
