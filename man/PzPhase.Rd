% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{PzPhase}
\alias{PzPhase}
\title{Eq (3.73).  The (M - N) angle(z_0) term accounts for the zeros or}
\usage{
PzPhase(z_0, alpha_k, beta_k, N = NULL, M = NULL) 
}
\arguments{
\item{z_0}{Coerced to complex by the body, with \code{as.complex}.}
\item{alpha_k}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{beta_k}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{N}{Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.}
\item{M}{Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.}
}
\description{
poles at the origin that balance the orders; dropping it is easy, since it vanishes when N = M, and leaves the phase wrong by a multiple of angle(z_0) whenever they differ.
}
\value{
A list with \code{phase}, \code{wrapped}, \code{origin_term}, \code{zero_angle_sum}, \code{pole_angle_sum}, \code{z_0_angle}, \code{n_zeros}, \code{n_poles}, \code{origin_term_vanishes_when_orders_match}, \code{method}.
}
\keyword{internal}
\examples{
PzPhase(z_0 = 5L, alpha_k = c(1, 2, 3, 4, 5, 6, 7, 8), beta_k = c(1, 2, 3, 4, 5, 6, 7, 8))
}
