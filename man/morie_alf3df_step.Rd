% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_alf3df_step}
\alias{morie_alf3df_step}
\title{One step of the sampler, from noise level t down to sigma_next}
\usage{
morie_alf3df_step(x, t, score_fn, sigma_next = NULL, gamma = 0, noise_scale = 1, step_scale = 1, order = "heun", e = NULL, augment = FALSE)
}
\arguments{
\item{x}{Current coordinates, one row per atom, three columns.}
\item{t}{The current noise level.}
\item{score_fn}{The denoiser returning an estimate of the CLEAN   coordinates. Note the parameterisation: not a score, not a noise   prediction.}
\item{sigma_next}{The level to step to; zero when NULL.}
\item{gamma}{Churn. Zero is deterministic.}
\item{noise_scale}{Scale on the injected noise. The AlphaFold-3   values live in Supplementary Methods 3.7 and are not reproduced   here, so this defaults to one -- the plain EDM sampler.}
\item{step_scale}{Scale on the step itself, likewise.}
\item{order}{"heun" applies the second-order correction.}
\item{e}{A random stream, needed for churn or augmentation.}
\item{augment}{Whether to centre and rotate first.}
}
\description{
One step of the sampler, from noise level t down to sigma_next
}
\value{
A list with the stepped coordinates, the churned level and   the direction.
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
oracle <- function(x, sigma) TRUEX
start <- matrix(0, NA_, 3L)
morie_alf3df_step(start, 4, oracle, 1, order = "euler")
}
