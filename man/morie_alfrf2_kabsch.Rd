% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_alfrf2_kabsch}
\alias{morie_alfrf2_kabsch}
\title{The rigid motion that best takes P onto Q, and the residual}
\usage{
morie_alfrf2_kabsch(P, Q)
}
\arguments{
\item{P}{The moving points, one row each.}
\item{Q}{The reference points.}
}
\description{
The rotation is the orthogonal polar factor of the cross-covariance C = sum q p', obtained as C (C'C)^{-1/2} from the eigendecomposition of C'C. If that factor comes out with a negative determinant it is a reflection, not a rotation, and the sign is flipped on the direction of least variance -- the standard correction, and the reason a naive superposition can silently report a mirror image as a match.
}
\value{
A list with the rotation, the translation, the root-mean-   square deviation and the moved points.
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
