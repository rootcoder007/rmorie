% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{EngCap}
\alias{EngCap}
\title{EngCap: ENG compound action potential from a fibre-velocity}
\usage{
EngCap(t, distance_m = 0.1, n_fibers = 40, cv_range = c(45, 70), amp_range = c(0.5, 2), width_ms = 0.3)
}
\arguments{
\item{t}{See Usage.}
\item{distance_m}{Defaults to \code{0.1}.}
\item{n_fibers}{Defaults to \code{40}.}
\item{cv_range}{Defaults to \code{c(45, 70)}.}
\item{amp_range}{Defaults to \code{c(0.5, 2)}.}
\item{width_ms}{Defaults to \code{0.3}.}
}
\description{
population Part of the rangayyan_phys implementation; see the file header for the source it follows.
}
\value{
A list with \code{t_ms}, \code{cap_uV}, \code{peak_uV}, \code{peak_latency_ms}, \code{onset_latency_ms}, \code{cv_from_peak_m_s}, \code{cv_from_onset_m_s}, \code{latencies_ms}, \code{velocities_m_s}, \code{distance_m}, \code{units}, \code{method}.
}
\keyword{internal}
