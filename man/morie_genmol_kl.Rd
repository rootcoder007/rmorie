% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_genmol_kl}
\alias{morie_genmol_kl}
\title{The Gaussian KL to a standard normal, in closed form}
\usage{
morie_genmol_kl(mu, logvar) 
}
\arguments{
\item{mu}{The posterior means.}
\item{logvar}{The posterior log-variances.}
}
\description{
Half the sum of the squared mean, plus the variance, minus one, minus the log variance. Exactly zero at mu zero and logvar zero, which is what makes it usable as a regulariser: the penalty is nothing when the posterior already is the prior.
}
\value{
A non-negative number.
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
morie_genmol_kl(V, V)
}
