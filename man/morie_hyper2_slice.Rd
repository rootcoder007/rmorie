% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_hyper2_slice}
\alias{morie_hyper2_slice}
\title{Neal's univariate slice sampler: stepping out, then shrinkage}
\usage{
morie_hyper2_slice(logf, x0, e, w = 1, m = 10L) 
}
\arguments{
\item{logf}{The log target.}
\item{x0}{Current value.}
\item{e}{A generator environment from .ghc_rng.}
\item{w}{Stepping-out width.}
\item{m}{Maximum stepping-out steps.}
}
\description{
No step size to tune: w only sets the unit the stepping-out uses, and the procedure is correct for any positive value of it.
}
\value{
The next value.
}
\references{
  Murray, I. and Adams, R.P. (2010) "Slice sampling covariance
    hyperparameters of latent Gaussian models." NIPS 23, 1732-1740.
  Murray, I., Adams, R.P. and MacKay, D.J.C. (2010) "Elliptical slice
    sampling." AISTATS 9, 541-548.
  Neal, R.M. (2003) "Slice sampling." Annals of Statistics 31(3),
    705-767.
  Rasmussen, C.E. and Williams, C.K.I. (2006) "Gaussian Processes for
    Machine Learning." MIT Press, chapters 2 and 4.
}
\keyword{internal}
\examples{
e <- .ghc_rng(11)
xs <- 3
e <- .ghc_rng(11)
xs <- 3
e <- .ghc_rng(11)
v <- 0
morie_hyper2_slice(function(v) -0.5 * v * v, xs, e, 1, 10L)
}
