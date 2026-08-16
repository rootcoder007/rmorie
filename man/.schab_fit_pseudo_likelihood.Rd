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
\item{z}{A vector; its length is taken.}
\item{X}{A matrix; passed to \code{nrow}.}
\item{Sigma_S}{A matrix; passed to \code{dim}.}
\item{family}{Passed to \code{.schab_canonical_link}. Defaults to \code{"poisson"}.}
\item{link_kind}{Optional; may be \code{NULL}. Passed to \code{.schab_pseudo_data}.}
\item{sigma2}{Passed to \code{.schab_sigma_mu}. Defaults to \code{1}.}
\item{R}{Passed to \code{.schab_sigma_mu}.}
\item{max_iter}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{100L}.}
\item{tol}{Defaults to \code{1e-08}.}
}
\description{
A step of the schab_glmm_shared implementation. Called by \code{sppql}. See the file header for the source the module follows.
}
\value{
A list with \code{beta}, \code{S}, \code{mu}, \code{sigma2}, \code{cov_beta}, \code{se_beta}, \code{Sigma_nu}, \code{pseudo_data}, \code{n_iter}, \code{converged}, \code{link}, \code{family}.
}
\keyword{internal}
\examples{
.schab_fit_pseudo_likelihood(z = 5L, X = 5L, Sigma_S = 5L)
}
