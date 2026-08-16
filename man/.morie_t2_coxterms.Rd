% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_t2_coxterms}
\alias{.morie_t2_coxterms}
\title{.morie_t2_coxterms}
\usage{
.morie_t2_coxterms(time, event, X, beta)
}
\arguments{
\item{time}{A vector; its length is taken and its elements indexed.}
\item{event}{A vector; indexed elementwise.}
\item{X}{A matrix; indexed by row and column.}
\item{beta}{A matrix; passed to \code{\%*\%}.}
}
\description{
A step of the tail2_t03 implementation. Called by \code{CoxPL}. See the file header for the source the module follows.
}
\value{
A list with \code{loglik}, \code{score}, \code{info}, \code{nevent}.
}
\keyword{internal}
