% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_diffRC_posterior_mean}
\alias{morie_diffRC_posterior_mean}
\title{DDPM posterior mean}
\usage{
morie_diffRC_posterior_mean(x_t, x0_hat, alpha_bar_t, alpha_bar_prev, beta_t) 
}
\arguments{
\item{x_t}{Numeric vector.}
\item{x0_hat}{Numeric vector, the model\'s estimate of x0.}
\item{alpha_bar_t}{Numeric.}
\item{alpha_bar_prev}{Numeric.}
\item{beta_t}{Numeric.}
}
\description{
DDPM posterior mean
}
\value{
A list with the mean and the two coefficients.
}
\keyword{internal}
\examples{
r <- morie_diffRC_posterior_mean(x_t = c(0.8, -0.9), x0_hat = c(1, -1),
                                 alpha_bar_t = 0.7, alpha_bar_prev = 0.8,
                                 beta_t = 0.1)
str(r, max.level = 1)
}
