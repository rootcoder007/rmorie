% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_alf3df_rotation}
\alias{morie_alf3df_rotation}
\title{A uniform random rotation matrix by Shoemake's quaternion method}
\usage{
morie_alf3df_rotation(e) 
}
\arguments{
\item{e}{A random stream from the shared generator.}
}
\description{
Three uniforms in, one rotation out, with no rejection step -- so it consumes exactly three draws whatever it returns and two implementations stay in step.
}
\value{
A three by three rotation matrix.
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
e <- rmorie:::.ghc_rng(5)
e <- rmorie:::.ghc_rng(5)
morie_alf3df_rotation(e)
}
