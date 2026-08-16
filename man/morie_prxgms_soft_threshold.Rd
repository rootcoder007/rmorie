% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_prxgms_soft_threshold}
\alias{morie_prxgms_soft_threshold}
\title{morie_prxgms_soft_threshold}
\usage{
morie_prxgms_soft_threshold(v, tau)
}
\arguments{
\item{v}{A vector; indexed elementwise.}
\item{tau}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the prxgms_native implementation. Called by \code{morie_prxgms_lasso_fista}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_prxgms_soft_threshold(V, V)
}
