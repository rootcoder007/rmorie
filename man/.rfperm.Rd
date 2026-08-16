% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.rfperm}
\alias{.rfperm}
\title{Out-of-bag permutation VIM, Chapter 15 pp. 642-643: "the values of}
\usage{
.rfperm(trees, oob, X, Y, q, normalise = TRUE)
}
\arguments{
\item{trees}{A vector; its length is taken and its elements indexed.}
\item{oob}{A vector; indexed elementwise.}
\item{X}{A matrix; indexed by row and column.}
\item{Y}{A matrix; indexed by row and column.}
\item{q}{Accepted by the signature and not used anywhere in the body.}
\item{normalise}{A flag; the body branches on it. Defaults to \code{TRUE}.}
}
\description{
the jth variable are randomly permuted in the OOB observations and j new PE is computed.  The differences between the two are then averaged over all the trees, and normalized by the standard deviation of the differences."  The permutation is the deterministic reversal of the OOB row order.
}
\value{
The value of \code{imp}, as built in the body.
}
\keyword{internal}
