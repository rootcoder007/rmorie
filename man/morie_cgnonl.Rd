% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_cgnonl}
\alias{morie_cgnonl}
\title{morie_cgnonl}
\usage{
morie_cgnonl(f, grad, x0, beta = "fletcher-reeves", restart = NULL, max_iter = NULL, tol = 1e-10, est = NULL, line_search = "fletcher-reeves", hess_vec = NULL, keep_path = FALSE)
}
\arguments{
\item{f}{See Usage.}
\item{grad}{See Usage.}
\item{x0}{See Usage.}
\item{beta}{Defaults to \code{"fletcher-reeves"}.}
\item{restart}{Defaults to \code{NULL}.}
\item{max_iter}{Defaults to \code{NULL}.}
\item{tol}{Defaults to \code{1e-10}.}
\item{est}{Defaults to \code{NULL}.}
\item{line_search}{Defaults to \code{"fletcher-reeves"}.}
\item{hess_vec}{Defaults to \code{NULL}.}
\item{keep_path}{Defaults to \code{FALSE}.}
}
\description{
Part of the cgnonl_native implementation; see the file header for the source it follows.
}
\value{
The value of \code{nonlinear_cg}.
}
\keyword{internal}
