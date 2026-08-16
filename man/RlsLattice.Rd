% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{RlsLattice}
\alias{RlsLattice}
\title{Section 8.6.2.  Every stage is itself a predictor, so one run gives}
\usage{
RlsLattice(x, order = 4, lam = 0.98, delta = 0.01)
}
\arguments{
\item{x}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{order}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{4}.}
\item{lam}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.98}.}
\item{delta}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.01}.}
}
\description{
the fit at EVERY order up to the one requested -- an order need not be chosen in advance, which is the lattice\'s advantage over the direct form.  |gamma| < 1 at every stage is the stability condition, the same one as eq. (7.39).
}
\value{
A list with \code{reflection}, \code{forward_error}, \code{backward_error}, \code{all_orders_forward}, \code{order}, \code{lam}, \code{stable}, \code{every_stage_is_a_predictor}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
RlsLattice(V)
}
