% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.chol_r}
\alias{.chol_r}
\title{.chol_r}
\usage{
.chol_r(A)
}
\arguments{
\item{A}{A matrix; indexed by row and column.}
}
\description{
A step of the bayopt_native implementation. Called by \code{gp_posterior}, \code{gp_posterior_gradient}. See the file header for the source the module follows.
}
\value{
The value of \code{L}, as built in the body.
}
\keyword{internal}
