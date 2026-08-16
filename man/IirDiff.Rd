% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{IirDiff}
\alias{IirDiff}
\title{IirDiff}
\usage{
IirDiff(x, b_k, a_k = NULL, y = NULL, N = NULL, M = NULL, n = NULL)
}
\arguments{
\item{x}{See Usage.}
\item{b_k}{See Usage.}
\item{a_k}{Defaults to \code{NULL}.}
\item{y}{Defaults to \code{NULL}.}
\item{N}{Defaults to \code{NULL}.}
\item{M}{Defaults to \code{NULL}.}
\item{n}{Defaults to \code{NULL}.}
}
\description{
A step of the rangayyan_filt implementation. Called by \code{IirDiffGen}. See the file header for the source the module follows.
}
\value{
A list with \code{y}, \code{value}, \code{index}, \code{N}, \code{M}, \code{recursive}, \code{feedback_is_subtracted}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
IirDiff(V, V)
}
