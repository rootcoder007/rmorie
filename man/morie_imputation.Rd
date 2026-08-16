% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_imputation}
\alias{morie_imputation}
\title{The two-step imputation estimator of Sec. 4}
\usage{
morie_imputation(Y, unit, period, first_treated, X = NULL)
}
\description{
Fit the two-way model on **untreated** observations only, impute the untreated potential outcome everywhere, and average the residual within each \eqn{(g, t)}{(g, t)} cell.  Sec. 5 proves this is numerically identical to \code{morie_etwfe}.
}
\value{
A named list mirroring the RichResult payload: \code{estimate},   \code{att}, \code{n_cells}, \code{n_untreated_used}, \code{coef},   \code{method}, \code{identical_to}.
}
\keyword{internal}
