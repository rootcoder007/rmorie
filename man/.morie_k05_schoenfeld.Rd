% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_k05_schoenfeld}
\alias{.morie_k05_schoenfeld}
\title{.morie_k05_schoenfeld}
\usage{
.morie_k05_schoenfeld(t, e, X, beta)
}
\arguments{
\item{t}{A vector; its length is taken and its elements indexed.}
\item{e}{A vector; indexed elementwise.}
\item{X}{A matrix; indexed by row and column.}
\item{beta}{A matrix; passed to \code{\%*\%}.}
}
\description{
A step of the k05_tranche2 implementation. Called by \code{morie_scaled_schoenfeld}. See the file header for the source the module follows.
}
\value{
A list with \code{times}, \code{res}, \code{var}.
}
\keyword{internal}
