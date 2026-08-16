% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.fluctuate}
\alias{.fluctuate}
\title{.fluctuate}
\usage{
.fluctuate(outcome, offset_logit, H, rows, iters = 100, tol = 1e-12)
}
\arguments{
\item{outcome}{A vector; indexed elementwise.}
\item{offset_logit}{A vector; indexed elementwise.}
\item{H}{A vector; indexed elementwise.}
\item{rows}{A vector; its length is taken.}
\item{iters}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{100}.}
\item{tol}{Defaults to \code{1e-12}.}
}
\description{
A step of the tmldyn_native implementation. Called by \code{morie_tmldyn}. See the file header for the source the module follows.
}
\value{
The value of \code{e}, as built in the body.
}
\keyword{internal}
