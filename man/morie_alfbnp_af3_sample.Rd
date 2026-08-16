% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_alfbnp_af3_sample}
\alias{morie_alfbnp_af3_sample}
\title{morie_alfbnp_af3_sample}
\usage{
morie_alfbnp_af3_sample(n_atoms = NULL, denoiser = NULL, clean = NULL, steps = 20L, sigma_data = 16, s_max = 160, s_min = 4e-04, rho = 7, gamma_0 = 0.8, gamma_min = 1, noise_scale = 1.003, step_scale = 1.5, noise = NULL, seed = 2, x_init = NULL, ridge = 1e-06)
}
\arguments{
\item{n_atoms}{Defaults to \code{NULL}.}
\item{denoiser}{Defaults to \code{NULL}.}
\item{clean}{Optional; may be \code{NULL}. Passed to \code{.alfbnp_clean_list}.}
\item{steps}{Defaults to \code{20L}.}
\item{sigma_data}{Compared against \code{"fit"}. Defaults to \code{16}.}
\item{s_max}{Defaults to \code{160}.}
\item{s_min}{Defaults to \code{4e-04}.}
\item{rho}{Defaults to \code{7}.}
\item{gamma_0}{Defaults to \code{0.8}.}
\item{gamma_min}{Defaults to \code{1}.}
\item{noise_scale}{Defaults to \code{1.003}.}
\item{step_scale}{Defaults to \code{1.5}.}
\item{noise}{Defaults to \code{NULL}.}
\item{seed}{Defaults to \code{2}.}
\item{x_init}{Optional; may be \code{NULL}. Passed to \code{.alfbnp_atoms}.}
\item{ridge}{Defaults to \code{1e-06}.}
}
\description{
A step of the alfbnp_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{coords}, \code{sigmas}, \code{trace}, \code{denoiser_coefs}, \code{sigma_data}, \code{steps}, \code{rmsd_to_reference}, \code{n_atoms}, \code{route}, \code{method}, \code{note}.
}
\keyword{internal}
