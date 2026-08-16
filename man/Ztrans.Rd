% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Ztrans}
\alias{Ztrans}
\title{Eq (3.54): X(z) = sum_n x(n) z^-n; eq (3.55) is the causal FIR case}
\usage{
Ztrans(x, z = NULL, n0 = 0)
}
\arguments{
\item{x}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{z}{Optional; may be \code{NULL}. Coerced to complex by the body, with \code{as.complex}.}
\item{n0}{Numeric; combined arithmetically in the body. Defaults to \code{0}.}
}
\description{
A step of the rangayyan_xfrm implementation. Called by \code{Zfm}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
Ztrans(V)
}
