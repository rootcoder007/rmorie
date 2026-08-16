% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.solve_kkt}
\alias{.solve_kkt}
\title{.solve_kkt}
\usage{
.solve_kkt(hmat, grad, aeq)
}
\arguments{
\item{hmat}{A matrix; passed to \code{solve}.}
\item{grad}{A vector; its length is taken.}
\item{aeq}{Optional; may be \code{NULL}. A matrix; passed to \code{nrow}.}
}
\description{
A step of the barerp_native implementation. Called by \code{central_point}. See the file header for the source the module follows.
}
\value{
The value of \code{[}.
}
\keyword{internal}
