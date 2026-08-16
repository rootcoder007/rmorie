% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{BiDomain}
\alias{BiDomain}
\title{BiDomain: 1-D monodomain propagation with the bidomain extracellular}
\usage{
BiDomain(n_nodes = 100, dx_cm = 0.02, duration_ms = 60, dt_ms = 0.005, sigma_i = 1, sigma_e = 2, C_m = 1, Sv = 1000, I_ion = NULL, stim_nodes = 5, I_stim = 50, stim_ms = 1, v_rest = -85, v_peak = 20, I_ion_peak = 10, threshold_frac = 0.25)
}
\arguments{
\item{n_nodes}{Defaults to \code{100}.}
\item{dx_cm}{Defaults to \code{0.02}.}
\item{duration_ms}{Defaults to \code{60}.}
\item{dt_ms}{Defaults to \code{0.005}.}
\item{sigma_i}{Defaults to \code{1}.}
\item{sigma_e}{Defaults to \code{2}.}
\item{C_m}{Defaults to \code{1}.}
\item{Sv}{Defaults to \code{1000}.}
\item{I_ion}{Defaults to \code{NULL}.}
\item{stim_nodes}{Defaults to \code{5}.}
\item{I_stim}{Defaults to \code{50}.}
\item{stim_ms}{Defaults to \code{1}.}
\item{v_rest}{Defaults to \code{-85}.}
\item{v_peak}{Defaults to \code{20}.}
\item{I_ion_peak}{Defaults to \code{10}.}
\item{threshold_frac}{Defaults to \code{0.25}.}
}
\description{
field; Rangayyan (2024) eqs. (7.143)-(7.149), Section 7.8.2.
}
\value{
A list with \code{x_cm}, \code{Vm_mV}, \code{phi_e_mV}, \code{phi_i_mV}, \code{Im_uA_cm2}, \code{activation_ms}, \code{n_activated}, \code{cv_cm_per_ms}, \code{D_cm2_per_ms}, \code{sigma_bulk_mS_cm}, \code{dt_ms}, \code{dx_cm}, \code{stability_limit_ms}, \code{units}, \code{method}.
}
\keyword{internal}
