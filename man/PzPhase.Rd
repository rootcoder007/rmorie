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
\item{z_0}{See Usage.}
\item{alpha_k}{See Usage.}
\item{beta_k}{See Usage.}
\item{N}{Defaults to \code{NULL}.}
\item{M}{Defaults to \code{NULL}.}
}
\description{
poles at the origin that balance the orders; dropping it is easy, since it vanishes when N = M, and leaves the phase wrong by a multiple of angle(z_0) whenever they differ.
}
\value{
A list with \code{phase}, \code{wrapped}, \code{origin_term}, \code{zero_angle_sum}, \code{pole_angle_sum}, \code{z_0_angle}, \code{n_zeros}, \code{n_poles}, \code{origin_term_vanishes_when_orders_match}, \code{method}.
}
\keyword{internal}
