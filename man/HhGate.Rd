% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{HhGate}
\alias{HhGate}
\title{HhGate: Hodgkin-Huxley gating variables m, h, n at a clamped}
\usage{
HhGate(V, dt = 0.01, m = NULL, h = NULL, n = NULL, steps = 1L)
}
\arguments{
\item{V}{See Usage.}
\item{dt}{Defaults to \code{0.01}.}
\item{m}{Defaults to \code{NULL}.}
\item{h}{Defaults to \code{NULL}.}
\item{n}{Defaults to \code{NULL}.}
\item{steps}{Defaults to \code{1L}.}
}
\description{
potential Part of the rangayyan_phys implementation; see the file header for the source it follows.
}
\value{
A list with \code{V_mV}, \code{dt_ms}, \code{steps}, \code{m}, \code{h}, \code{n}, \code{m_inf}, \code{h_inf}, \code{n_inf}, \code{tau_m_ms}, \code{tau_h_ms}, \code{tau_n_ms}, \code{alpha_per_ms}, \code{beta_per_ms}, \code{units}, \code{method}.
}
\keyword{internal}
\examples{
HhGate(V = 5L)
}
