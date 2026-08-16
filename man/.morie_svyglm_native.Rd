% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_svyglm_native}
\alias{.morie_svyglm_native}
\title{Internal helper: svyglm-equivalent weighted GLM with linearized SEs}
\usage{
.morie_svyglm_native(formula, data, weights, family = stats::gaussian())
}
\description{
Internal helper: svyglm-equivalent weighted GLM with linearized SEs
}
\value{
A list with \code{coefficients}, \code{confint}, \code{vcov}, \code{fit}.
}
\keyword{internal}
