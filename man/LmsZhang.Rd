% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{LmsZhang}
\alias{LmsZhang}
\title{Eq. (3.205), after Zhang et al.: mu(n) = mu / ((M+1) xbar^2(n)) with}
\usage{
LmsZhang(mu, order, r, alpha = 0.02, power_prev = NULL)
}
\arguments{
\item{mu}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{order}{Coerced to integer by the body, with \code{as.integer}.}
\item{r}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{alpha}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.02}.}
\item{power_prev}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
xbar^2(n) = alpha r^2(n) + (1-alpha) xbar^2(n-1).  Normalizing by the running power is what makes the step scale-free, so one mu works across a record whose amplitude varies by an order of magnitude -- the book\'s motivation for VAG signals.  alpha must be small: near 1 it tracks the instantaneous sample and reintroduces the very jitter the averaging removes, so a value above 0.5 is refused.
}
\value{
A list with \code{mu}, \code{power}, \code{power_prev}, \code{alpha}, \code{order}, \code{base_mu}, \code{method}.
}
\keyword{internal}
\examples{
LmsZhang(0.5, 4, 2, alpha = 0.02)
}
