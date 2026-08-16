% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_did_twfe_native}
\alias{.morie_did_twfe_native}
\title{Internal helper: TWFE OLS with fixest-style CR1 cluster vcov}
\usage{
.morie_did_twfe_native(y, X, unit, time, cluster_ids)
}
\description{
Fits y on X with unit + time fixed effects absorbed, returning the slope coefficients and cluster-robust standard errors under fixest\'s default small-sample correction: `(n-1)/(n-K) * G/(G-1)` where K counts the slopes plus the fixed-effect coefficients not nested in the cluster variable (fixef.K = "nested").
}
\value{
A list with \code{beta}, \code{se}, \code{vcov}, \code{keep}, \code{residuals}, \code{n}, \code{n_clusters}, \code{df_t}, \code{n_units}, \code{n_periods}.
}
\keyword{internal}
