% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_numeric_cov}
\alias{.morie_numeric_cov}
\title{Inverse of the central-difference Hessian. The optimiser\\\'s own}
\usage{
.morie_numeric_cov(fn, theta, rel = 1e-05)
}
\arguments{
\item{fn}{See Usage.}
\item{theta}{See Usage.}
\item{rel}{Defaults to \code{1e-05}.}
}
\description{
inverse-Hessian is a secant approximation accumulated along whatever path it walked, so it is not reproducible across solvers; this is.
}
\value{
The value of \code{tryCatch}.
}
\keyword{internal}
