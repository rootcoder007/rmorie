% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.gpzapbestsplit}
\alias{.gpzapbestsplit}
\title{.gpzapbestsplit}
\usage{
.gpzapbestsplit(y, x, candidates = NULL)
}
\arguments{
\item{y}{Passed to \code{.gpflat}.}
\item{x}{Passed to \code{.gpflat}.}
\item{candidates}{Optional; may be \code{NULL}. Passed to \code{.gpflat}.}
}
\description{
A step of the helpers_gp_core implementation. Called by \code{Msm325}. See the file header for the source the module follows.
}
\value{
A list with \code{threshold}, \code{loglik}.
}
\keyword{internal}
