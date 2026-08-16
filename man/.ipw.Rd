% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.ipw}
\alias{.ipw}
\title{.ipw}
\usage{
.ipw(y, w, e, keep = NULL, weights = NULL)
}
\arguments{
\item{y}{A vector; its length is taken and its elements indexed.}
\item{w}{A vector; indexed elementwise.}
\item{e}{A vector; indexed elementwise.}
\item{keep}{Optional; may be \code{NULL}. Passed to \code{is.null}.}
\item{weights}{Optional; may be \code{NULL}. Passed to \code{is.null}.}
}
\description{
A step of the tmlefp_native implementation. Called by \code{morie_tmlefp}. See the file header for the source the module follows.
}
\value{
A list with \code{est}, \code{n_kept}.
}
\keyword{internal}
