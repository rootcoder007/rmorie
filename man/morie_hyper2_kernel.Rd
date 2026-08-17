% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_hyper2_kernel}
\alias{morie_hyper2_kernel}
\title{Covariance between two sets of inputs}
\usage{
morie_hyper2_kernel(X, Z, log_ls, log_sf, kind = "squared_exponential")
}
\arguments{
\item{X}{First input matrix.}
\item{Z}{Second input matrix.}
\item{log_ls}{log lengthscale.}
\item{log_sf}{log signal standard deviation.}
\item{kind}{A member of the kernel list.}
}
\description{
Covariance between two sets of inputs
}
\value{
The covariance matrix.
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
set.seed(1)
X <- matrix(rnorm(6), 3, 2)
morie_hyper2_kernel(X, X, log_ls = 0, log_sf = 0)
}
