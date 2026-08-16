% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.cgnonl_exact_quadratic_step}
\alias{.cgnonl_exact_quadratic_step}
\title{.cgnonl_exact_quadratic_step}
\usage{
.cgnonl_exact_quadratic_step(x, p, g, hess_vec)
}
\arguments{
\item{x}{See Usage.}
\item{p}{Passed to \code{.cgnonl_dot}.}
\item{g}{Passed to \code{.cgnonl_dot}.}
\item{hess_vec}{See Usage.}
}
\description{
A step of the cgnonl_native implementation. Called by \code{nonlinear_cg}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
