% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.potM_neg_loglik}
\alias{.potM_neg_loglik}
\title{.potM_neg_loglik}
\usage{
.potM_neg_loglik(par, z)
}
\arguments{
\item{par}{A vector; indexed elementwise.}
\item{z}{Passed to \code{.potM_loglik}.}
}
\description{
A step of the potM_mixedcase_native implementation. Called by \code{.potM_gpd_mle}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.potM_neg_loglik(V, V)
}
