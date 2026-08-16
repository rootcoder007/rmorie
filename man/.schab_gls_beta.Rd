% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.schab_gls_beta}
\alias{.schab_gls_beta}
\title{.schab_gls_beta}
\usage{
.schab_gls_beta(X, Sigma_nu, nu)
}
\arguments{
\item{X}{A matrix; passed to \code{t}.}
\item{Sigma_nu}{A matrix; passed to \code{as.matrix}.}
\item{nu}{A matrix; passed to \code{\%*\%}.}
}
\description{
A step of the schab_glmm_shared implementation. Called by \code{.schab_fit_pseudo_likelihood}. See the file header for the source the module follows.
}
\value{
A list with \code{beta}, \code{cov_beta}.
}
\keyword{internal}
