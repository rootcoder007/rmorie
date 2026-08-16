% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{CorSound}
\alias{CorSound}
\title{CorSound: coronary artery sound generation model (turbulent flow);}
\usage{
CorSound(diameter, flow_velocity, stenosis_pct = 0, p2max = 1, freqs = NULL, nu = 3.5e-06)
}
\arguments{
\item{diameter}{See Usage.}
\item{flow_velocity}{See Usage.}
\item{stenosis_pct}{Defaults to \code{0}.}
\item{p2max}{Defaults to \code{1}.}
\item{freqs}{Defaults to \code{NULL}.}
\item{nu}{Defaults to \code{3.5e-06}.}
}
\description{
Rangayyan (2024) eqs. (7.135)-(7.136), Section 7.7.2.
}
\value{
A list with \code{freq_hz}, \code{psd_Pa2_per_Hz}, \code{D_normal_m}, \code{d_stenotic_m}, \code{U_normal_m_s}, \code{u_stenotic_m_s}, \code{stenosis_pct}, \code{corner_freq_hz}, \code{reynolds_param_x}, \code{reynolds_number}, \code{total_power_Pa2}, \code{units}, \code{method}.
}
\keyword{internal}
\examples{
CorSound(diameter = 5L, flow_velocity = 5L)
}
