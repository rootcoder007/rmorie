% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.hyper2_elliptical}
\alias{.hyper2_elliptical}
\title{Elliptical slice sampling for a latent with prior N(0, L L\\'). The}
\usage{
.hyper2_elliptical(logl, f, L, e)
}
\arguments{
\item{logl}{Accepted by the signature and not used anywhere in the body.}
\item{f}{A vector; its length is taken.}
\item{L}{A matrix; indexed by row and column.}
\item{e}{Passed to \code{.ghc_norm}.}
}
\description{
proposal is an exact ellipse through the current point and a fresh prior draw, so the prior term cancels and only the likelihood enters the acceptance test. No rejection and no step size.
}
\value{
The value of \code{f}, as built in the body.
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
