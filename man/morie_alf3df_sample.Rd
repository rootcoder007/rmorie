% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_alf3df_sample}
\alias{morie_alf3df_sample}
\title{Run the whole schedule from noise to a structure}
\usage{
morie_alf3df_sample(shape_n, score_fn, n_steps = 8L, sigma_min = 0.002, sigma_max = 80, 
    rho = 7, gamma = 0, noise_scale = 1, step_scale = 1, order = "heun", 
    seed = 0, augment = FALSE) 
}
\arguments{
\item{shape_n}{The number of atoms.}
\item{score_fn}{The denoiser.}
\item{n_steps}{Schedule length.}
\item{sigma_min}{The smallest positive level.}
\item{sigma_max}{The starting level.}
\item{rho}{The schedule exponent.}
\item{gamma}{Churn.}
\item{noise_scale}{Scale on the injected noise.}
\item{step_scale}{Scale on the step.}
\item{order}{A member of the order list.}
\item{seed}{The random stream.}
\item{augment}{Whether to centre and rotate at each step.}
}
\description{
Run the whole schedule from noise to a structure
}
\value{
A list with the final coordinates, the schedule and the   churned levels visited.
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
shrink <- function(x, sigma) (1/(1 + sigma)) * x
morie_alf3df_sample(NA_, shrink, 5L, seed = 3)
}
