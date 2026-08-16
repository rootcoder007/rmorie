% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.schab_predict_random_field}
\alias{.schab_predict_random_field}
\title{.schab_predict_random_field}
\usage{
.schab_predict_random_field(Sigma_S, Sigma_nu, nu, X, beta)
}
\arguments{
\item{Sigma_S}{A matrix; passed to \code{as.matrix}.}
\item{Sigma_nu}{A matrix; passed to \code{as.matrix}.}
\item{nu}{See Usage.}
\item{X}{A matrix; passed to \code{as.matrix}.}
\item{beta}{See Usage.}
}
\description{
A step of the schab_glmm_shared implementation. Called by \code{.schab_fit_pseudo_likelihood}. See the file header for the source the module follows.
}
\value{
A vector, from \code{as.numeric}.
}
\keyword{internal}
