% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.schab_marginal_moments_lognormal}
\alias{.schab_marginal_moments_lognormal}
\title{.schab_marginal_moments_lognormal}
\usage{
.schab_marginal_moments_lognormal(X, beta, sigma2_S, sigma2 = 1, rho = NULL)
}
\arguments{
\item{X}{A matrix; passed to \code{\%*\%}.}
\item{beta}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{sigma2_S}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{sigma2}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1}.}
\item{rho}{Optional; may be \code{NULL}. A matrix; passed to \code{as.matrix}.}
}
\description{
A step of the schab_glmm_shared implementation. Called by \code{spglmm}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
