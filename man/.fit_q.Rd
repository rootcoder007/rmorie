% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.fit_q}
\alias{.fit_q}
\title{.fit_q}
\usage{
.fit_q(y, A, W, levels, rows, ridge)
}
\arguments{
\item{y}{A vector; its length is taken and its elements indexed.}
\item{A}{A vector; indexed elementwise.}
\item{W}{A matrix; indexed by row and column.}
\item{levels}{A vector; indexed elementwise.}
\item{rows}{See Usage.}
\item{ridge}{See Usage.}
}
\description{
A step of the tmldta_native implementation. Called by \code{discover_levels}, \code{split_specific_tmle}. See the file header for the source the module follows.
}
\value{
A list with \code{q}, \code{b}.
}
\keyword{internal}
