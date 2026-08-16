% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_cypin_fit}
\alias{morie_cypin_fit}
\title{Logistic regression by iteratively reweighted least squares}
\usage{
morie_cypin_fit(X, y, ridge = 1e-06, iters = 50L, tol = 1e-12)
}
\arguments{
\item{X}{One descriptor row per compound.}
\item{y}{The inhibition labels.}
\item{ridge}{The penalty on the slopes.}
\item{iters}{The iteration cap.}
\item{tol}{The convergence tolerance on the coefficient step.}
}
\description{
An intercept is prepended, so the returned vector is one longer than a descriptor row and its first entry is the intercept. The ridge is a small quadratic penalty on the slopes and not on the intercept. It is there because inhibition data is routinely separable -- every compound above some lipophilicity inhibits -- and a separable logistic fit has no finite maximum, so without it the coefficients run off to infinity and the iteration reports a number that only means "it kept going". The penalty is a parameter and it is reported back.
}
\value{
A list with the coefficients, the deviance, the iteration   count and the score.
}
\references{
  Veith, H. et al. (2009) "Comprehensive characterization of
    cytochrome P450 isozyme selectivity across chemical libraries."
    Nature Biotechnology 27(11), 1050-1055. doi:10.1038/nbt.1581.
  Meija, J. et al. (2016) "Atomic weights of the elements 2013."
    Pure and Applied Chemistry 88(3), 265-291.
  McCullagh, P. and Nelder, J.A. (1989) "Generalized Linear Models",
    2nd edition, Chapman and Hall.
}
\keyword{internal}
