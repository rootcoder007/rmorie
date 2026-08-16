% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.abcgp.chol_solve}
\alias{.abcgp.chol_solve}
\title{.abcgp.chol_solve}
\usage{
.abcgp.chol_solve(L, b)
}
\arguments{
\item{L}{A matrix; indexed by row and column.}
\item{b}{A vector; its length is taken and its elements indexed.}
}
\description{
A step of the abcgp_native implementation. Called by \code{.abcgp.gp_fit}, \code{.abcgp.gp_predict}, \code{.abcgp.mvn_logpdf} and 1 others in the module. See the file header for the source the module follows.
}
\value{
The value of \code{x}, as built in the body.
}
\keyword{internal}
