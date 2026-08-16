% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{is_curriculum}
\alias{is_curriculum}
\title{is_curriculum}
\usage{
is_curriculum(weights, p = NULL, tol = 1e-12)
}
\arguments{
\item{weights}{A vector; its length is taken and its elements indexed.}
\item{p}{Optional; may be \code{NULL}. A vector; indexed elementwise.}
\item{tol}{Numeric; combined arithmetically in the body. Defaults to \code{1e-12}.}
}
\description{
A step of the prgrl_native implementation. Called by \code{prgrl}. See the file header for the source the module follows.
}
\value{
A list with \code{is_curriculum}, \code{entropy_increasing}, \code{strictly_increasing}, \code{weights_monotone}, \code{final_step_is_p}, \code{entropies}.
}
\keyword{internal}
