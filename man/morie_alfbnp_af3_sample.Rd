% Generated from the module source by ledger/gen_rd.R -- roxygen2 hangs
% on this package. Usage comes from args(), so it tracks the formals.
\name{morie_alfbnp_af3_sample}
\alias{morie_alfbnp_af3_sample}
\alias{morie_alfbnp}
\title{AlphaFold-3 SampleDiffusion (Abramson et al. 2024)}
\usage{
morie_alfbnp_af3_sample(n_atoms = NULL, denoiser = NULL, clean = NULL, steps = 20L, sigma_data = 16, s_max = 160, s_min = 4e-04, rho = 7, gamma_0 = 0.8, gamma_min = 1, noise_scale = 1.003, step_scale = 1.5, noise = NULL, seed = 2, x_init = NULL, ridge = 1e-06)
}
\description{
R arm of alfbnp -- AlphaFold-3 SampleDiffusion (Abramson et al. 2024).   sigma_data 16, s_max 160, s_min 4e-4, rho 7, gamma_0 0.8, gamma_min 1.0,   noise_scale 1.003, step_scale 1.5.
}
\value{
A list with components \code{sigma_data}, \code{estimate}, \code{coords}, \code{sigmas}, \code{trace}, \code{denoiser_coefs}, \code{steps}, \code{rmsd_to_reference}, \code{n_atoms}, \code{route}, \code{method}, \code{note}.
}
\references{
Abramson, J. et al. (2024) Nature 630(8016), 493-500,
  doi:10.1038/s41586-024-07487-w. Algorithm 18 and the Table 6 defaults:
Karras, T. et al. (2022) NeurIPS 35, 26565-26577 -- the sigma schedule.
Vincent, P. (2011) Neural Computation 23(7), 1661-1674 -- the fit objective.
}
\keyword{internal}
