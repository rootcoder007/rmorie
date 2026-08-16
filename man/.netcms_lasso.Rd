% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.netcms_lasso}
\alias{.netcms_lasso}
\title{.netcms_lasso}
\usage{
.netcms_lasso(V, s12, lam, beta, tol, maxit)
}
\arguments{
\item{V}{A matrix; indexed by row and column.}
\item{s12}{A vector; its length is taken and its elements indexed.}
\item{lam}{Passed to \code{.netcms_soft}.}
\item{beta}{A vector; indexed elementwise.}
\item{tol}{See Usage.}
\item{maxit}{A count; the body uses it as \code{seq_len(...)}.}
}
\description{
A step of the netcms_native implementation. Called by \code{morie_netcms}. See the file header for the source the module follows.
}
\value{
The value of \code{beta}, as built in the body.
}
\keyword{internal}
\examples{
.netcms_lasso(V = matrix(c(1, 2, 3, 4, 5, 6), nrow = 2), s12 = 5L, lam = c(1, 2, 3, 4, 5, 6, 7, 8), beta = 0.5, tol = 0.5, maxit = 5L)
}
