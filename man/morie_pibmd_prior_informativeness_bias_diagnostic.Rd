% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_pibmd_prior_informativeness_bias_diagnostic}
\alias{morie_pibmd_prior_informativeness_bias_diagnostic}
\title{morie_pibmd_prior_informativeness_bias_diagnostic}
\usage{
morie_pibmd_prior_informativeness_bias_diagnostic(samples, prior, n_grid = 512L)
}
\arguments{
\item{samples}{See Usage.}
\item{prior}{A list; the body reads \code{$mean}, \code{$sd} from it.}
\item{n_grid}{Defaults to \code{512L}.}
}
\description{
A step of the pibmd_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{kl_divergence}, \code{kl_divergence_kde}, \code{kl_divergence_reverse}, \code{kl_symmetric}, \code{shrinkage}, \code{bias_in_prior_sd}, \code{wasserstein_1}, \code{conflict_p_value}, \code{conflict_p_value_gaussian}, \code{conflict_p_value_empirical}, \code{posterior_mean}, \code{posterior_var}, \code{posterior_sd}, \code{prior_mean}, \code{prior_var}, \code{prior_sd}, \code{n_posterior}, \code{n_prior}, \code{moments_only}, \code{verdict}, \code{informativeness}, \code{method}, \code{note}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_pibmd_prior_informativeness_bias_diagnostic(V, V)
}
