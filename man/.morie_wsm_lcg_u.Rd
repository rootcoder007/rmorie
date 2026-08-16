% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_wsm_lcg_u}
\alias{.morie_wsm_lcg_u}
\title{.morie_wsm_lcg_u}
\usage{
.morie_wsm_lcg_u(count, seed = 13)
}
\arguments{
\item{count}{A count; the body uses it as \code{seq_len(...)}.}
\item{seed}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{13}.}
}
\description{
A step of the wasserman_native implementation. Called by \code{.morie_wsm_boot_reps}, \code{morie_wasserman_gibbs_sampler}, \code{morie_wasserman_mcmc_metropolis} and 1 others in the module. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
