% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_alfrf2}
\alias{morie_alfrf2}
\title{Grow a backbone around a fixed motif by reverse diffusion}
\usage{
morie_alfrf2(target_motif, scaffold, T = 20L, denoise = "ideal", denoiser = NULL, beta_start = 1e-04, beta_end = 0.02, spacing = .alfrf2_ca_spacing, passes = 8L, noise_scale = 1, seed = 0)
}
\arguments{
\item{target_motif}{A list of pairs of a zero-based residue index and   the three coordinates that residue must end up at.}
\item{scaffold}{The number of residues in the design, or a starting   structure to noise from.}
\item{T}{Diffusion steps.}
\item{denoise}{Either ideal or prior; see the file header on what   each is and is not.}
\item{denoiser}{A function of the structure and the step returning   the predicted clean structure. Given, it replaces the routes.}
\item{beta_start,beta_end}{The variance schedule endpoints.}
\item{spacing}{The backbone separation the ideal route relaxes to.}
\item{passes}{Relaxation passes per denoising call.}
\item{noise_scale}{A multiplier on the reverse-process noise. One is   the sampler as published; zero makes the walk deterministic.}
\item{seed}{The random stream.}
}
\description{
Grow a backbone around a fixed motif by reverse diffusion
}
\value{
A list with the designed backbone, the motif it was built   around, the motif RMSD -- which must be zero -- and the chain   geometry it came out with.
}
\references{
  Watson, J.L. et al. (2023) "De novo design of protein structure and
    function with RFdiffusion." Nature 620, 1089-1100.
    doi:10.1038/s41586-023-06415-8.
  Ho, J., Jain, A. and Abbeel, P. (2020) "Denoising diffusion
    probabilistic models." NeurIPS 33, 6840-6851.
  Lugmayr, A. et al. (2022) "RePaint: inpainting using denoising
    diffusion probabilistic models." CVPR, 11461-11471.
  Kabsch, W. (1976) "A solution for the best rotation to relate two
    sets of vectors." Acta Crystallographica A32(5), 922-923.
}
\keyword{internal}
