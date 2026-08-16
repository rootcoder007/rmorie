% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_rg_solve}
\alias{.morie_rg_solve}
\title{.morie_rg_solve}
\usage{
.morie_rg_solve(A, b)
}
\arguments{
\item{A}{A matrix; passed to \code{solve}.}
\item{b}{A matrix; passed to \code{solve}.}
}
\description{
A step of the rangayyan_adapt implementation. Called by \code{PcgSeg}, \code{WienerHopf}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
F <- matrix(0.9)
H <- matrix(1)
Q <- matrix(0.1)
R <- matrix(1)
Riccati(F, H, Q, R)
.morie_rg_solve(A = F, b = F)
}
