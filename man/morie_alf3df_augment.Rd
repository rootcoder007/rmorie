% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_alf3df_augment}
\alias{morie_alf3df_augment}
\title{Recentre on the centroid, then rotate at random}
\usage{
morie_alf3df_augment(x, e)
}
\arguments{
\item{x}{A coordinate matrix, one row per atom.}
\item{e}{A random stream.}
}
\description{
The centring is exact: the returned centroid is zero to rounding, not approximately zero. The rotation preserves every interatomic distance, which is the property that makes the augmentation harmless to the structure and useful to the model.
}
\value{
A list with the augmented coordinates and the old centroid.
}
\references{
  Abramson, J., Adler, J., Dunger, J., Evans, R., Green, T., Pritzel,
    A., Ronneberger, O., Willmore, L., Ballard, A.J., Bambrick, J. et
    al. (2024) "Accurate structure prediction of biomolecular
    interactions with AlphaFold 3." Nature 630(8016), 493-500.
    doi:10.1038/s41586-024-07487-w.
  Karras, T., Aittala, M., Aila, T. and Laine, S. (2022) "Elucidating
    the design space of diffusion-based generative models." Advances
    in Neural Information Processing Systems 35, 26565-26577.
    arXiv:2206.00364. Equation 5 and Algorithm 2.
  Song, Y. et al. (2021) "Score-based generative modeling through
    stochastic differential equations." ICLR.
  Shoemake, K. (1992) "Uniform random rotations." Graphics Gems III,
    124-132.
}
\keyword{internal}
\examples{
NA_ <- 6L
TRUEX <- matrix(0, NA_, 3L)
e <- .ghc_rng(5)
e <- .ghc_rng(5)
morie_alf3df_augment(TRUEX, e)
}
