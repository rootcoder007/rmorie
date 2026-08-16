% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_alfrf2_schedule}
\alias{morie_alfrf2_schedule}
\title{The linear variance schedule and its cumulative products}
\usage{
morie_alfrf2_schedule(T, beta_start = 1e-04, beta_end = 0.02)
}
\arguments{
\item{T}{The number of steps.}
\item{beta_start}{The first variance.}
\item{beta_end}{The last variance.}
}
\description{
Returns betas, alphas and alpha-bars indexed from step one, with the zeroth alpha-bar equal to one standing for the clean structure so the forward and reverse formulas need no special case at the ends. The endpoints are the ones Ho et al. use; RFdiffusion's own schedule is a parameter here rather than a constant, because it is a training choice and not a property of diffusion.
}
\value{
A list with betas, alphas and abar.
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
\examples{
morie_alfrf2_schedule(T = 5L)
}
