% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{breakdown_value}
\alias{breakdown_value}
\title{breakdown_value}
\usage{
breakdown_value(beta, n_pre, n_post, family = "SD", l_vec = NULL, sign = "positive", M_max = 10, tol = 1e-09)
}
\arguments{
\item{beta}{See Usage.}
\item{n_pre}{See Usage.}
\item{n_post}{See Usage.}
\item{family}{Defaults to \code{"SD"}.}
\item{l_vec}{Defaults to \code{NULL}.}
\item{sign}{One of \code{"negative"}, \code{"positive"}. Defaults to \code{"positive"}.}
\item{M_max}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{10}.}
\item{tol}{Defaults to \code{1e-09}.}
}
\description{
A step of the snmtst_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{breakdown}, \code{family}, \code{sign}, \code{status}.
}
\keyword{internal}
