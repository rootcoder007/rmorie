% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.rn_mc_p_fitted}
\alias{.rn_mc_p_fitted}
\title{Internal: Monte Carlo null of the KS statistic with fitted Gaussian}
\usage{
.rn_mc_p_fitted(d_obs, n, n_mc)
}
\arguments{
\item{d_obs}{See Usage.}
\item{n}{See Usage.}
\item{n_mc}{A count; the body uses it as \code{seq_len(...)}.}
}
\description{
parameters (the Lilliefors construction; parameter-free for a fitted location-scale family).
}
\value{
A numeric value.
}
\keyword{internal}
