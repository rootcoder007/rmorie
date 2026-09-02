% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_msm_cox_weighted}
\alias{morie_msm_cox_weighted}
\title{morie_msm_cox_weighted}
\usage{
morie_msm_cox_weighted(time, event, treatment_history, weights = NULL, n_iter = 60, 
    tol = 1e-10) 
}
\description{
morie_msm_cox_weighted
}
\arguments{
\item{time}{Argument \code{time}; see Usage.}
\item{event}{Argument \code{event}; see Usage.}
\item{treatment_history}{Argument \code{treatment_history}; see Usage.}
\item{weights}{Argument \code{weights}; see Usage.}
\item{n_iter}{Argument \code{n_iter}; see Usage.}
\item{tol}{Argument \code{tol}; see Usage.}
}
\value{
A list with \code{beta}, \code{hazard_ratio}.
}
\keyword{internal}
\examples{
rmorie:::morie_msm_cox_weighted(time = 5L, event = c(0, 1, 0, 1, 1, 0, 1, 0), treatment_history = matrix(c(1, 2, 3, 4, 5, 6), nrow = 2))
}
