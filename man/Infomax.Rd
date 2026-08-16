% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Infomax}
\alias{Infomax}
\title{Infomax}
\usage{
Infomax(X, ncomp = NULL, eta = 0.05, maxiter = 300, tol = 1e-08, seed = 1)
}
\arguments{
\item{X}{Passed to \code{.morie_bx_mat}.}
\item{ncomp}{Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.}
\item{eta}{Numeric; combined arithmetically in the body. Defaults to \code{0.05}.}
\item{maxiter}{A count; the body uses it as \code{seq_len(...)}. Defaults to \code{300}.}
\item{tol}{Defaults to \code{1e-08}.}
\item{seed}{Passed to \code{.morie_bx_rng}. Defaults to \code{1}.}
}
\description{
A step of the rangayyan_class2 implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{sources}, \code{unmixing}, \code{whitening}, \code{mean}, \code{iterations}, \code{change}, \code{method}.
}
\keyword{internal}
