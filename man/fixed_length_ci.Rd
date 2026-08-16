% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{fixed_length_ci}
\alias{fixed_length_ci}
\title{fixed_length_ci}
\usage{
fixed_length_ci(beta, sigma, n_pre, n_post, M = 0, family = "SD", l_vec = NULL, level = 0.95)
}
\arguments{
\item{beta}{See Usage.}
\item{sigma}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{n_pre}{See Usage.}
\item{n_post}{See Usage.}
\item{M}{Defaults to \code{0}.}
\item{family}{Defaults to \code{"SD"}.}
\item{l_vec}{Defaults to \code{NULL}.}
\item{level}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.95}.}
}
\description{
A step of the snmtst_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{lower}, \code{upper}, \code{identified_lower}, \code{identified_upper}, \code{M}, \code{family}, \code{level}, \code{conservative}, \code{method}.
}
\keyword{internal}
