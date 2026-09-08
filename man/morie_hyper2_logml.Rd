% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_hyper2_logml}
\alias{morie_hyper2_logml}
\title{log N(y; 0, K + s2n I), by Cholesky}
\usage{
morie_hyper2_logml(y, X, log_ls, log_sf, log_sn, kind) 
}
\arguments{
\item{y}{Targets.}
\item{X}{Inputs.}
\item{log_ls}{log lengthscale.}
\item{log_sf}{log signal standard deviation.}
\item{log_sn}{log noise standard deviation.}
\item{kind}{Kernel name.}
}
\description{
The form that never builds an inverse: -0.5 y' A^-1 y minus the sum of the log diagonal of the factor minus n/2 log(2 pi).
}
\value{
The log marginal likelihood.
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
