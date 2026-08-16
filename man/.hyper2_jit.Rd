% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.hyper2_jit}
\alias{.hyper2_jit}
\title{.hyper2_jit}
\usage{
.hyper2_jit(K, v)
}
\arguments{
\item{K}{A matrix; indexed by row and column.}
\item{v}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the hyper2_native implementation. Called by \code{morie_hyper2}, \code{morie_hyper2_logml}. See the file header for the source the module follows.
}
\value{
The value of \code{K}, as built in the body.
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
