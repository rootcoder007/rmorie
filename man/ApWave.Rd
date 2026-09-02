% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{ApWave}
\alias{ApWave}
\title{ApWave: idealised action-potential waveform (ramp and decay)}
\usage{
ApWave(t, v_rest = -70, v_peak = 30, t_rise = 0.5, t_fall = 1, t_onset = 0, 
    v_undershoot = NULL, t_recover = NULL) 
}
\arguments{
\item{t}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{v_rest}{Numeric; combined arithmetically in the body. Defaults to \code{-70}.}
\item{v_peak}{Numeric; combined arithmetically in the body. Defaults to \code{30}.}
\item{t_rise}{Numeric; combined arithmetically in the body. Defaults to \code{0.5}.}
\item{t_fall}{Numeric; combined arithmetically in the body. Defaults to \code{1}.}
\item{t_onset}{Numeric; combined arithmetically in the body. Defaults to \code{0}.}
\item{v_undershoot}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{t_recover}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the rangayyan_phys implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{t_ms}, \code{V_mV}, \code{amplitude_mV}, \code{peak_time_ms}, \code{v_rest_mV}, \code{v_peak_mV}, \code{t_rise_ms}, \code{t_fall_ms}, \code{width_half_ms}, \code{units}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
ApWave(V)
}
