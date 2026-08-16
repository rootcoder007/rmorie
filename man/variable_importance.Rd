% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{variable_importance}
\alias{variable_importance}
\title{variable_importance}
\usage{
variable_importance(X, Y, screen, effect, V = 5L, seed = 0L)
}
\arguments{
\item{X}{A matrix; passed to \code{as.matrix}.}
\item{Y}{Accepted by the signature and not used anywhere in the body.}
\item{screen}{Accepted by the signature and not used anywhere in the body.}
\item{effect}{Accepted by the signature and not used anywhere in the body.}
\item{V}{Defaults to \code{5L}.}
\item{seed}{Defaults to \code{0L}.}
}
\description{
A step of the tldapar_native implementation. Called by \code{morie_tldapar}. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{importance}, \code{V}, \code{method}.
}
\keyword{internal}
