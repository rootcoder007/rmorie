% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_genmol_elbo}
\alias{morie_genmol_elbo}
\title{The evidence lower bound: reconstruction minus the divergence}
\usage{
morie_genmol_elbo(reconstruction, mu, logvar, beta = 1) 
}
\arguments{
\item{reconstruction}{The reconstruction term.}
\item{mu,logvar}{The posterior parameters.}
\item{beta}{The weight on the divergence.}
}
\description{
Beta weights the divergence. At one this is the bound; above one it is the beta-VAE trade, which buys a more disentangled latent space at the cost of reconstruction, and it is a parameter because that trade is the caller's to make.
}
\value{
The bound.
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
\examples{
morie_genmol_elbo(reconstruction = c(1, 2, 3, 4, 5, 6, 7, 8), mu = c(1, 2, 3, 4, 5, 6, 7, 8),
  logvar = c(1, 2, 3, 4, 5, 6, 7, 8))
}
