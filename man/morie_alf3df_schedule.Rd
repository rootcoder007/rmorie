% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_alf3df_schedule}
\alias{morie_alf3df_schedule}
\title{The EDM noise schedule, Karras et al. equation 5}
\usage{
morie_alf3df_schedule(n_steps, sigma_min = 0.002, sigma_max = 80, rho = 7)
}
\arguments{
\item{n_steps}{The number of noise levels before the final zero.}
\item{sigma_min}{The smallest positive level.}
\item{sigma_max}{The starting level.}
\item{rho}{The schedule exponent.}
}
\description{
rho controls how much of the schedule is spent at low noise: larger puts more steps near the end, where the structure is actually being decided.
}
\value{
The levels, with a zero appended.
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
