% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.blocks}
\alias{.blocks}
\title{.blocks}
\usage{
.blocks(covariate_history, n)
}
\arguments{
\item{covariate_history}{Optional; may be \code{NULL}. Coerced to list by the body, with \code{as.list}.}
\item{n}{Passed to \code{!=}.}
}
\description{
A step of the tmldyn_native implementation. Called by \code{morie_tmldyn}. See the file header for the source the module follows.
}
\value{
A list with \code{L0}, \code{L1}.
}
\keyword{internal}
