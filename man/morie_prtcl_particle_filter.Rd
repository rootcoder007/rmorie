% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_prtcl_particle_filter}
\alias{morie_prtcl_particle_filter}
\title{morie_prtcl_particle_filter}
\usage{
morie_prtcl_particle_filter(y, n.particles, init, step, loglik, seed = 0L, resample.threshold = 1, 
    systematic = TRUE) 
}
\arguments{
\item{y}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{n.particles}{Coerced to integer by the body, with \code{as.integer}.}
\item{init}{Accepted by the signature and not used anywhere in the body.}
\item{step}{Accepted by the signature and not used anywhere in the body.}
\item{loglik}{Accepted by the signature and not used anywhere in the body.}
\item{seed}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{0L}.}
\item{resample.threshold}{Numeric; combined arithmetically in the body. Defaults to \code{1}.}
\item{systematic}{A flag; the body branches on it. Defaults to \code{TRUE}.}
}
\description{
A step of the prtcl_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{filtered.mean}, \code{loglik}, \code{ess}, \code{min.ess}, \code{resampled}, \code{n.particles}, \code{n.obs}, \code{systematic}, \code{particles}, \code{method}.
}
\keyword{internal}
