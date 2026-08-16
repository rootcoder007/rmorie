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
\item{trees}{See Usage.}
\item{oob}{See Usage.}
\item{X}{See Usage.}
\item{Y}{See Usage.}
\item{q}{See Usage.}
\item{normalise}{Defaults to \code{TRUE}.}
}
\description{
the jth variable are randomly permuted in the OOB observations and j new PE is computed.  The differences between the two are then averaged over all the trees, and normalized by the standard deviation of the differences."  The permutation is the deterministic reversal of the OOB row order.
}
\value{
The value of \code{imp}, as built in the body.
}
\keyword{internal}
