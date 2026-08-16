% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.alfrf2_centre}
\alias{.alfrf2_centre}
\title{.alfrf2_centre}
\usage{
.alfrf2_centre(P)
}
\arguments{
\item{P}{A matrix; indexed by row and column.}
}
\description{
A step of the alfrf2_native implementation. Called by \code{morie_alfrf2_kabsch}. See the file header for the source the module follows.
}
\value{
A list with \code{P}, \code{c}.
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
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
.alfrf2_centre(M)
}
