% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_msm_cox_weighted}
\alias{morie_msm_cox_weighted}
\title{morie_msm_cox_weighted}
\usage{
morie_msm_cox_weighted(time, event, treatment_history, weights = NULL, n_iter = 60, tol = 1e-10)
}
\description{
morie_msm_cox_weighted
}
\value{
A list with \code{beta}, \code{hazard_ratio}.
}
\keyword{internal}
\examples{
morie_msm_cox_weighted(time = 5L, event = c(0, 1, 0, 1, 1, 0, 1, 0), treatment_history = matrix(c(1, 2, 3, 4, 5, 6), nrow = 2))
}
