% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{HhModel}
\alias{HhModel}
\title{HhModel: four-variable Hodgkin-Huxley membrane model, RK4}
\usage{
HhModel(duration = 30, dt = 0.01, I_ext = 10, stim_start = 5, stim_stop = 6, C_m = 1, g_Na = 120, g_K = 36, g_L = 0.3, E_Na = 50, E_K = -77, E_L = -54.387, V0 = -65)
}
\arguments{
\item{duration}{Defaults to \code{30}.}
\item{dt}{Defaults to \code{0.01}.}
\item{I_ext}{Defaults to \code{10}.}
\item{stim_start}{Defaults to \code{5}.}
\item{stim_stop}{Defaults to \code{6}.}
\item{C_m}{Defaults to \code{1}.}
\item{g_Na}{Defaults to \code{120}.}
\item{g_K}{Defaults to \code{36}.}
\item{g_L}{Defaults to \code{0.3}.}
\item{E_Na}{Defaults to \code{50}.}
\item{E_K}{Defaults to \code{-77}.}
\item{E_L}{Defaults to \code{-54.387}.}
\item{V0}{Defaults to \code{-65}.}
}
\description{
Part of the rangayyan_phys implementation; see the file header for the source it follows.
}
\value{
A list with \code{t_ms}, \code{V_mV}, \code{m}, \code{h}, \code{n}, \code{I_Na_uA_cm2}, \code{I_K_uA_cm2}, \code{I_L_uA_cm2}, \code{peak_mV}, \code{peak_time_ms}, \code{rest_mV}, \code{min_mV}, \code{spiked}, \code{n_spikes}, \code{dt_ms}, \code{I_ext_uA_cm2}, \code{units}, \code{method}.
}
\keyword{internal}
\examples{
HhModel()
}
