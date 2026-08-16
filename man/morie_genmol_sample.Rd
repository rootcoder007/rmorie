% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_genmol_sample}
\alias{morie_genmol_sample}
\title{Reparameterised draws: z = mu + temperature * sigma * epsilon}
\usage{
morie_genmol_sample(mu, logvar, n = 1L, temperature = 1, seed = 0)
}
\arguments{
\item{mu,logvar}{The posterior parameters.}
\item{n}{How many draws.}
\item{temperature}{The multiplier on the standard deviation.}
\item{seed}{The random stream.}
}
\description{
At temperature zero every draw is exactly the mean -- not approximately, exactly -- which is the check that the noise really is entering through this one multiplication and nowhere else.
}
\value{
A list of latent vectors.
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
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_genmol_sample(V, V)
}
