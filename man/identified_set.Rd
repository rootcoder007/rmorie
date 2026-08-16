% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{identified_set}
\alias{identified_set}
\title{identified_set}
\usage{
identified_set(beta, n_pre, n_post, M = 0, family = "SD", l_vec = NULL, grid = NULL)
}
\arguments{
\item{beta}{See Usage.}
\item{n_pre}{See Usage.}
\item{n_post}{See Usage.}
\item{M}{Defaults to \code{0}.}
\item{family}{Defaults to \code{"SD"}.}
\item{l_vec}{Defaults to \code{NULL}.}
\item{grid}{Defaults to \code{NULL}.}
}
\description{
Part of the snmtst_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{lower}, \code{upper}, \code{estimate}, \code{pre_max_change}, \code{bound}, \code{M}, \code{family}, \code{width}, \code{note}.
}
\keyword{internal}
