% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_iv_kclass_native}
\alias{.morie_iv_kclass_native}
\title{Internal helper: k-class IV estimator with robust variance}
\usage{
.morie_iv_kclass_native(y, X, Z, kappa = 1, robust = TRUE)
}
\description{
beta(k) = (X\'(I - k M_Z) X)^{-1} X\'(I - k M_Z) y where M_Z is the annihilator of the instrument set. k = 1 gives 2SLS, k = 0 OLS, and the LIML eigenvalue gives LIML. Robust (HC1) variance uses the projected regressors as scores, matching sandwich::vcovHC(ivreg-fit, "HC1").
}
\value{
A list with \code{beta}, \code{se}, \code{vcov}, \code{residuals}, \code{n}, \code{df}, \code{kappa}, \code{PzX}.
}
\keyword{internal}
