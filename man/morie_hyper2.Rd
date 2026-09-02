% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_hyper2}
\alias{morie_hyper2}
\title{Posterior over GP hyperparameters, predictions averaged over it}
\usage{
morie_hyper2(X, y, prior = NULL, kind = "squared_exponential", route = "marginal", 
    n_iter = 200L, burn = NULL, thin = 1L, seed = 1, Xstar = NULL, w = 1, 
    m = 10L, jitter = 1e-08) 
}
\arguments{
\item{X}{Training inputs.}
\item{y}{Training targets.}
\item{prior}{Starting values for the three log-hyperparameters. The   prior itself is standard normal on each; this argument only sets   where the chain begins, and is named prior because the ledger's   signature calls it that.}
\item{kind}{Kernel name.}
\item{route}{"marginal", "whitened" or "surrogate".}
\item{n_iter}{Chain length.}
\item{burn}{Burn-in; half of n_iter by default.}
\item{thin}{Thinning.}
\item{seed}{Seed for the generator shared with the Python arm.}
\item{Xstar}{Test inputs; the training inputs by default.}
\item{w}{Slice-sampling stepping-out width.}
\item{m}{Slice-sampling step limit.}
\item{jitter}{Added to the diagonal for positive-definiteness.}
}
\description{
Posterior over GP hyperparameters, predictions averaged over it
}
\value{
A list with the hyperparameter draws, their posterior means   and standard deviations, and the predictive mean and standard   deviation at Xstar averaged over the draws.
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
X <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 3)
y <- c(1.5, 2.1, 2.9)
morie_hyper2(X, y)
}
