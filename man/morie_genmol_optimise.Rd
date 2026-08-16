% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_genmol_optimise}
\alias{morie_genmol_optimise}
\title{Gradient ascent on a property, in the latent space}
\usage{
morie_genmol_optimise(z0, property_fn, steps = 20L, lr = 0.1, eps = 1e-04)
}
\arguments{
\item{z0}{The starting latent.}
\item{property_fn}{The property to maximise.}
\item{steps}{How many ascent steps.}
\item{lr}{The step size.}
\item{eps}{The finite-difference spacing.}
}
\description{
The gradient is a central difference, so the property function need not be differentiable or even continuous -- which matters, because the properties worth optimising in chemistry are things like synthetic accessibility and predicted binding, and those come from code, not from formulas. The trajectory is returned, not just the endpoint: a latent optimisation that ran off to infinity looks exactly like a successful one if you only report where it stopped.
}
\value{
A list with the endpoint, the trajectory and the values.
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
prop <- function(z) -((z[1] - 1)^2 + (z[2] + 0.5)^2)
morie_genmol_optimise(c(-2, 3), prop, 20L, 0.1)
}
