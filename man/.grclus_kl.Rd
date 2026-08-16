% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.grclus_kl}
\alias{.grclus_kl}
\title{Refinement ----}
\usage{
.grclus_kl(adj, vw, parts, target, tolerance, boundary, max_passes, patience)
}
\arguments{
\item{adj}{A vector; its length is taken and its elements indexed.}
\item{vw}{A vector; indexed elementwise.}
\item{parts}{A vector; indexed elementwise.}
\item{target}{Numeric; combined arithmetically in the body.}
\item{tolerance}{Numeric; combined arithmetically in the body.}
\item{boundary}{A flag; the body branches on it.}
\item{max_passes}{See Usage.}
\item{patience}{See Usage.}
}
\description{
A step of the grclus_native implementation. Called by \code{.grclus_bisect}. See the file header for the source the module follows.
}
\value{
A list with \code{parts}, \code{cut}.
}
\keyword{internal}
