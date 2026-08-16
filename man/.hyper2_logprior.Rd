% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.hyper2_logprior}
\alias{.hyper2_logprior}
\title{Standard normal on each log-hyperparameter. A lognormal prior, which}
\usage{
.hyper2_logprior(theta)
}
\arguments{
\item{theta}{Numeric; combined arithmetically in the body.}
}
\description{
is proper -- an improper flat prior on a log lengthscale gives an improper posterior whenever the data cannot rule out an arbitrarily long one, and a sampler will wander off into that region without ever saying so.
}
\value{
The value of \code{.w3_csum}.
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
