% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_painn_equivariance_error}
\alias{morie_painn_equivariance_error}
\title{morie_painn_equivariance_error}
\usage{
morie_painn_equivariance_error(model, s, v, R, Q, tol = 1e-09)
}
\arguments{
\item{model}{Accepted by the signature and not used anywhere in the body.}
\item{s}{See Usage.}
\item{v}{Passed to \code{.painn_mat}.}
\item{R}{Passed to \code{.painn_mat}.}
\item{Q}{Passed to \code{.painn_mat}.}
\item{tol}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1e-09}.}
}
\description{
A step of the painn_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{scalar_error}, \code{vector_error}, \code{scalars_invariant}, \code{vectors_equivariant}, \code{note}.
}
\keyword{internal}
