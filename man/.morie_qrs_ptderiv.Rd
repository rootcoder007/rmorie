% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_qrs_ptderiv}
\alias{.morie_qrs_ptderiv}
\title{Pan-Tompkins derivative, eq (4.14)}
\usage{
.morie_qrs_ptderiv(x)
}
\arguments{
\item{x}{A vector; its length is taken and its elements indexed.}
}
\description{
A step of the rangayyan_qrs implementation. Called by \code{.morie_qrs_chain}, \code{QrsDerivOp}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.morie_qrs_ptderiv(V)
}
