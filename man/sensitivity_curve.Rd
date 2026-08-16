% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{sensitivity_curve}
\alias{sensitivity_curve}
\title{sensitivity_curve}
\usage{
sensitivity_curve(beta, n_pre, n_post, Ms, family = "SD", l_vec = NULL)
}
\arguments{
\item{beta}{See Usage.}
\item{n_pre}{See Usage.}
\item{n_post}{See Usage.}
\item{Ms}{Iterated over elementwise, with \code{lapply}.}
\item{family}{Defaults to \code{"SD"}.}
\item{l_vec}{Defaults to \code{NULL}.}
}
\description{
A step of the snmtst_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{curve}, \code{family}, \code{M}, \code{width}.
}
\keyword{internal}
