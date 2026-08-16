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
\item{histories}{A vector; its length is taken and its elements indexed.}
\item{propensities}{A vector; indexed elementwise.}
\item{stabilise}{A flag; the body branches on it. Defaults to \code{TRUE}.}
\item{marginal}{Optional; may be \code{NULL}. A vector; indexed elementwise.}
\item{truncate}{Defaults to \code{NULL}.}
}
\description{
A step of the bhltmsm_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{weights}, \code{raw}, \code{stabilised}, \code{truncated}, \code{n_truncated}, \code{note}.
}
\keyword{internal}
