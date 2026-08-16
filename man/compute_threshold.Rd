% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{compute_threshold}
\alias{compute_threshold}
\title{compute_threshold}
\usage{
compute_threshold(i1, i2, a1_new, a2_new, alpha, y, E, K, b, C)
}
\arguments{
\item{i1}{See Usage.}
\item{i2}{See Usage.}
\item{a1_new}{See Usage.}
\item{a2_new}{See Usage.}
\item{alpha}{See Usage.}
\item{y}{See Usage.}
\item{E}{A vector; indexed elementwise.}
\item{K}{A matrix; indexed by row and column.}
\item{b}{See Usage.}
\item{C}{See Usage.}
}
\description{
A step of the smoopt_native implementation. Called by \code{smo_platt}. See the file header for the source the module follows.
}
\value{
A list with \code{b}, \code{from}, \code{b1}, \code{b2}, \code{note}.
}
\keyword{internal}
