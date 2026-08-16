% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_linprm}
\alias{morie_linprm}
\title{morie_linprm}
\usage{
morie_linprm(c, A, b, tol = 1e-10, max_iter = 200)
}
\arguments{
\item{c}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{A}{Iterated over elementwise, with \code{lapply}.}
\item{b}{A vector; its length is taken and its elements indexed.}
\item{tol}{Defaults to \code{1e-10}.}
\item{max_iter}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{200}.}
}
\description{
A step of the linprm_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{x}, \code{y}, \code{s}, \code{iterations}, \code{gap}, \code{converged}, \code{mu_history}, \code{primal_residual}, \code{dual_residual}.
}
\keyword{internal}
