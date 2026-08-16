% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{treatment_weights}
\alias{treatment_weights}
\title{treatment_weights}
\usage{
treatment_weights(histories, propensities, stabilise = TRUE, marginal = NULL, truncate = NULL)
}
\arguments{
\item{histories}{See Usage.}
\item{propensities}{See Usage.}
\item{stabilise}{Defaults to \code{TRUE}.}
\item{marginal}{Defaults to \code{NULL}.}
\item{truncate}{Defaults to \code{NULL}.}
}
\description{
Part of the bhltmsm_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{weights}, \code{raw}, \code{stabilised}, \code{truncated}, \code{n_truncated}, \code{note}.
}
\keyword{internal}
