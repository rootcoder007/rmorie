% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.schab_fit_reml}
\alias{.schab_fit_reml}
\title{.schab_fit_reml}
\usage{
.schab_fit_reml(coords, z, X, model = "exponential", start_ratio = 0.1, start_range = NULL, max_iter = 200L, tol = 1e-10)
}
\arguments{
\item{coords}{See Usage.}
\item{z}{See Usage.}
\item{X}{See Usage.}
\item{model}{Defaults to \code{"exponential"}.}
\item{start_ratio}{Defaults to \code{0.1}.}
\item{start_range}{Defaults to \code{NULL}.}
\item{max_iter}{Defaults to \code{200L}.}
\item{tol}{Defaults to \code{1e-10}.}
}
\description{
Part of the schab_reml_shared implementation; see the file header for the source it follows.
}
\value{
A list with \code{nugget_ratio}, \code{range}, \code{sigma2}, \code{nugget}, \code{partial_sill}, \code{beta}, \code{neg2_restricted_loglik}, \code{converged}.
}
\keyword{internal}
