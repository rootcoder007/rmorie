% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.rslusolve}
\alias{.rslusolve}
\title{Solve A x = b; NULL when A is singular}
\usage{
.rslusolve(A, b)
}
\arguments{
\item{A}{A matrix; passed to \code{nrow}.}
\item{b}{A vector; indexed elementwise.}
}
\description{
A step of the helpers_rouss implementation. Called by \code{.rsltsfit}, \code{Lmsreg}, \code{Ltsreg} and 2 others in the module. See the file header for the source the module follows.
}
\value{
The value of \code{x}, as built in the body.
}
\keyword{internal}
