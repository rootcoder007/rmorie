% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{ShiftInt}
\alias{ShiftInt}
\title{Psi = E[ \{g(A - delta | H) / g(A | H)\} Y ] -- the density ratio at}
\usage{
ShiftInt(y, a, h, delta = 1, trim = NULL) 
}
\arguments{
\item{y}{Passed to \code{.morie_spx_chkv}.}
\item{a}{Passed to \code{.morie_spx_chkv}.}
\item{h}{Optional; may be \code{NULL}. A matrix; passed to \code{as.matrix}.}
\item{delta}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1}.}
\item{trim}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
the BACK-shifted exposure.  Forward-shifting is the sign error this estimand invites.  Gaussian working model A|H ~ N(H\'gamma, tau^2), so w = exp\{(delta/tau^2)(A - H\'gamma - delta/2)\}.  Diaz & van der Laan (2012, 2018).  NOT in Schabenberger & Gotway.
}
\value{
A list with \code{psi}, \code{naive_mean}, \code{weights}, \code{max_weight}, \code{mean_weight}, \code{tau2}, \code{gamma}, \code{delta}, \code{weight_uses_back_shifted_density}, \code{gaussian_working_model}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
set.seed(7)
h <- matrix(rnorm(30), 30, 1)
a <- 0.5 * h[, 1] + rnorm(30)
y <- a + h[, 1] + rnorm(30, 0, 0.3)
ShiftInt(y, a, h, delta = 0.5)
}
