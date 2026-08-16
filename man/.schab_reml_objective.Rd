% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.schab_reml_objective}
\alias{.schab_reml_objective}
\title{.schab_reml_objective}
\usage{
.schab_reml_objective(X, Sigma_nu, nu)
}
\arguments{
\item{X}{A matrix; passed to \code{nrow}.}
\item{Sigma_nu}{A matrix; passed to \code{as.matrix}.}
\item{nu}{A matrix; passed to \code{\%*\%}.}
}
\description{
A step of the schab_glmm_shared implementation. Called by \code{sppql}. See the file header for the source the module follows.
}
\value{
A vector, from \code{as.numeric}.
}
\keyword{internal}
\examples{
.schab_reml_objective(X = 5L, Sigma_nu = 5L, nu = 0.5)
}
