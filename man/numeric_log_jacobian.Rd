% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{numeric_log_jacobian}
\alias{numeric_log_jacobian}
\title{numeric_log_jacobian}
\usage{
numeric_log_jacobian(mapfun, z, h = 1e-06)
}
\arguments{
\item{mapfun}{Accepted by the signature and not used anywhere in the body.}
\item{z}{A vector; its length is taken and its elements indexed.}
\item{h}{Numeric; combined arithmetically in the body. Defaults to \code{1e-06}.}
}
\description{
A step of the bayrjmcmc_native implementation. Called by \code{reversible_jump_mcmc}. See the file header for the source the module follows.
}
\value{
The value of \code{.logabsdet}.
}
\keyword{internal}
\examples{
j <- numeric_log_jacobian(function(z) c(z[1] + z[2], z[1] - z[2]),
                          c(0.4, 0.2))
stopifnot(abs(j - log(2)) < 1e-5)
j
}
