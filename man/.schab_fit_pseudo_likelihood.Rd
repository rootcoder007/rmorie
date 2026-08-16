% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.schab_fit_pseudo_likelihood}
\alias{.schab_fit_pseudo_likelihood}
\title{.schab_fit_pseudo_likelihood}
\usage{
.schab_fit_pseudo_likelihood(z, X, Sigma_S, family = "poisson", link_kind = NULL, sigma2 = 1, R = NULL, max_iter = 100L, tol = 1e-08)
}
\arguments{
\item{z}{See Usage.}
\item{X}{See Usage.}
\item{Sigma_S}{See Usage.}
\item{family}{Defaults to \code{"poisson"}.}
\item{link_kind}{Defaults to \code{NULL}.}
\item{sigma2}{Defaults to \code{1}.}
\item{R}{Defaults to \code{NULL}.}
\item{max_iter}{Defaults to \code{100L}.}
\item{tol}{Defaults to \code{1e-08}.}
}
\description{
Part of the schab_glmm_shared implementation; see the file header for the source it follows.
}
\value{
A list with \code{beta}, \code{S}, \code{mu}, \code{sigma2}, \code{cov_beta}, \code{se_beta}, \code{Sigma_nu}, \code{pseudo_data}, \code{n_iter}, \code{converged}, \code{link}, \code{family}.
}
\keyword{internal}
