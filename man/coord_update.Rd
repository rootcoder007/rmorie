% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{coord_update}
\alias{coord_update}
\title{Eq. (4): x_i + C sum_j (x_i - x_j) phi_x(m_{ij})}
\usage{
coord_update(X, M, phi_x, C = NULL)
}
\arguments{
\item{X}{A vector; its length is taken and its elements indexed.}
\item{M}{A vector; indexed elementwise.}
\item{phi_x}{Accepted by the signature and not used anywhere in the body.}
\item{C}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the egnnL_native implementation. Called by \code{egcl}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
