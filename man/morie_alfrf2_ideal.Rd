% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_alfrf2_ideal}
\alias{morie_alfrf2_ideal}
\title{Relax consecutive alpha carbons toward the backbone spacing}
\usage{
morie_alfrf2_ideal(x, fixed, spacing = .alfrf2_ca_spacing, passes = 8L) 
}
\arguments{
\item{x}{The structure, one row per residue.}
\item{fixed}{The zero-based indices held in place.}
\item{spacing}{The target separation.}
\item{passes}{How many relaxation passes.}
}
\description{
A symmetric constraint relaxation: each consecutive pair is moved along the line joining it until the pair is the right distance apart, both ends sharing the correction equally unless one of them is pinned, in which case the free end takes all of it. Repeated for a fixed number of passes so the result does not depend on a convergence test. Two points on top of each other have no direction to be separated along, so that pair is left alone rather than moved somewhere arbitrary.
}
\value{
The relaxed structure.
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
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_alfrf2_ideal(V, V)
}
