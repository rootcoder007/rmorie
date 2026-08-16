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
\item{c}{See Usage.}
\item{A}{See Usage.}
\item{b}{See Usage.}
\item{tol}{Defaults to \code{1e-10}.}
\item{max_iter}{Defaults to \code{200}.}
}
\description{
Part of the linprm_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{x}, \code{y}, \code{s}, \code{iterations}, \code{gap}, \code{converged}, \code{mu_history}, \code{primal_residual}, \code{dual_residual}.
}
\keyword{internal}
