% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_genmol}
\alias{morie_genmol}
\title{Sample molecules from a latent model and score what came back}
\usage{
morie_genmol(model, n_samples, conditions = NULL, route = "vae", temperature = 1, seed = 0, steps = 20L, T = 10L, beta = 1)
}
\arguments{
\item{model}{A list with mu and logvar for the latent prior, and   decoder, a function from a latent vector to a SMILES string.   Without a decoder nothing is generated and the reason says so.}
\item{n_samples}{How many to draw.}
\item{conditions}{A list with property to optimise each latent   against before decoding, and training_set to measure novelty   against.}
\item{route}{Either vae or diffusion.}
\item{temperature}{The multiplier on the noise.}
\item{seed}{The random stream.}
\item{steps}{Ascent steps when a property is given.}
\item{T}{Diffusion steps for the diffusion route.}
\item{beta}{The weight on the divergence in the bound.}
}
\description{
Sample molecules from a latent model and score what came back
}
\value{
A list with the latents, the decoded strings, and validity,   uniqueness and novelty as fractions of the sample.
}
\references{
  Gomez-Bombarelli, R. et al. (2018) "Automatic chemical design using
    a data-driven continuous representation of molecules." ACS
    Central Science 4(2), 268-276. doi:10.1021/acscentsci.7b00572.
  Sanchez-Lengeling, B. and Aspuru-Guzik, A. (2018) "Inverse
    molecular design using machine learning." Science 361(6400),
    360-365. doi:10.1126/science.aat2663.
  Kingma, D.P. and Welling, M. (2014) "Auto-encoding variational
    Bayes." ICLR.
  Ho, J., Jain, A. and Abbeel, P. (2020) "Denoising diffusion
    probabilistic models." NeurIPS 33, 6840-6851.
  Polykovskiy, D. et al. (2020) "Molecular sets (MOSES): a
    benchmarking platform for molecular generation models."
    Frontiers in Pharmacology 11, 565644.
}
\keyword{internal}
