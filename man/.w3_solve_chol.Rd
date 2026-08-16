% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.w3_solve_chol}
\alias{.w3_solve_chol}
\title{Solve L L\\' x = b by forward then back substitution}
\usage{
.w3_solve_chol(lo, b)
}
\arguments{
\item{lo}{A matrix; indexed by row and column.}
\item{b}{A vector; indexed elementwise.}
}
\description{
A step of the helpers_w3num implementation. Called by \code{.w3_inv_from_chol}, \code{.w3_ols}, \code{morie_cypin_fit} and 3 others in the module. See the file header for the source the module follows.
}
\value{
The value of \code{x}, as built in the body.
}
\keyword{internal}
