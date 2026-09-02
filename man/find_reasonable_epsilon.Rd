% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{find_reasonable_epsilon}
\alias{find_reasonable_epsilon}
\title{Algorithm 4: double or halve the step size until the Metropolis}
\usage{
find_reasonable_epsilon(theta, logp, grad, rnd, eps = 1, max_doublings = 100) 
}
\arguments{
\item{theta}{A vector; its length is taken.}
\item{logp}{Passed to \code{.joint}.}
\item{grad}{Passed to \code{leapfrog}.}
\item{rnd}{Accepted by the signature and not used anywhere in the body.}
\item{eps}{Numeric; combined arithmetically in the body. Defaults to \code{1}.}
\item{max_doublings}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{100}.}
}
\description{
acceptance probability crosses one half. The argument `rnd` is the single-value normal generator (the closure around the shared RNG).
}
\value{
The value of \code{eps}, as built in the body.
}
\keyword{internal}
\examples{
logp <- function(t) -0.5 * sum(t^2)
grad <- function(t) -t
e <- .ghc_rng(1)
find_reasonable_epsilon(c(1, -1), logp, grad,
                        rnd = function() .ghc_norm(e, 1L))
}
