% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.potM_loglik}
\alias{.potM_loglik}
\title{.potM_loglik}
\usage{
.potM_loglik(z, sigma, xi)
}
\arguments{
\item{z}{A vector; its length is taken.}
\item{sigma}{Numeric; passed to \code{log}.}
\item{xi}{Numeric; passed to \code{abs}.}
}
\description{
A step of the potM_mixedcase_native implementation. Called by \code{.potM_gpd_mle}, \code{.potM_neg_loglik}. See the file header for the source the module follows.
}
\value{
One of two values, depending on the branch taken.
}
\keyword{internal}
