% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_bisect_q}
\alias{.morie_bisect_q}
\title{.morie_bisect_q}
\usage{
.morie_bisect_q(cdf, p, lo, hi, tol = 1e-13)
}
\arguments{
\item{cdf}{Accepted by the signature and not used anywhere in the body.}
\item{p}{Numeric; combined arithmetically in the body.}
\item{lo}{Numeric; combined arithmetically in the body.}
\item{hi}{Numeric; passed to \code{abs}.}
\item{tol}{Numeric; combined arithmetically in the body. Defaults to \code{1e-13}.}
}
\description{
A step of the dist_native implementation. Called by \code{Qbeta}, \code{Qf}, \code{Qgamma} and 1 others in the module. See the file header for the source the module follows.
}
\value{
The value of \code{x}, as built in the body.
}
\keyword{internal}
