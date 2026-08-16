% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_alfrf2_noise}
\alias{morie_alfrf2_noise}
\title{The forward process in closed form}
\usage{
morie_alfrf2_noise(x0, abar_t, eps)
}
\arguments{
\item{x0}{The clean structure, a matrix of coordinates.}
\item{abar_t}{The cumulative alpha at the step wanted.}
\item{eps}{Standard normal deviates of the same shape.}
}
\description{
x_t = sqrt(abar_t) x_0 + sqrt(1 - abar_t) eps. At abar equal to one this is the structure itself, which is what makes the motif exact at the end of the reverse walk.
}
\value{
The noised structure.
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
morie_alfrf2_noise(x0 = c(1, 2, 3, 4, 5, 6, 7, 8), abar_t = c(1, 2, 3, 4, 5, 6, 7, 8), eps = 0.5)
}
