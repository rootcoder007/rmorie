% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_nnls}
\alias{.morie_nnls}
\title{Non-negative least squares via projected coordinate descent --}
\usage{
.morie_nnls(A, b, tol = 1e-10, max_iter = 2000L)
}
\arguments{
\item{A}{See Usage.}
\item{b}{See Usage.}
\item{tol}{Defaults to \code{1e-10}.}
\item{max_iter}{Defaults to \code{2000L}.}
}
\description{
convergent for the convex NNLS objective and immune to the singular normal-equation solves that break active-set methods when learner predictions are collinear.
}
\value{
The value of \code{x}, as built in the body.
}
\keyword{internal}
