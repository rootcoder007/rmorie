% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{ApWave}
\alias{ApWave}
\title{ApWave: idealised action-potential waveform (ramp and decay)}
\usage{
ApWave(t, v_rest = -70, v_peak = 30, t_rise = 0.5, t_fall = 1, t_onset = 0, v_undershoot = NULL, t_recover = NULL)
}
\arguments{
\item{t}{See Usage.}
\item{v_rest}{Defaults to \code{-70}.}
\item{v_peak}{Defaults to \code{30}.}
\item{t_rise}{Defaults to \code{0.5}.}
\item{t_fall}{Defaults to \code{1}.}
\item{t_onset}{Defaults to \code{0}.}
\item{v_undershoot}{Defaults to \code{NULL}.}
\item{t_recover}{Defaults to \code{NULL}.}
}
\description{
Part of the rangayyan_phys implementation; see the file header for the source it follows.
}
\value{
A list with \code{t_ms}, \code{V_mV}, \code{amplitude_mV}, \code{peak_time_ms}, \code{v_rest_mV}, \code{v_peak_mV}, \code{t_rise_ms}, \code{t_fall_ms}, \code{width_half_ms}, \code{units}, \code{method}.
}
\keyword{internal}
