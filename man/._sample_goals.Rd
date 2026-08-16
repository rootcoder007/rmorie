% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{._sample_goals}
\alias{._sample_goals}
\title{._sample_goals}
\usage{
._sample_goals(strategy, episode, t, T, k, m, pool, e)
}
\arguments{
\item{strategy}{One of \code{"episode"}, \code{"final"}, \code{"future"}.}
\item{episode}{A vector; indexed elementwise.}
\item{t}{Numeric; combined arithmetically in the body.}
\item{T}{Numeric; combined arithmetically in the body.}
\item{k}{Passed to \code{.ghc_unif}.}
\item{m}{See Usage.}
\item{pool}{A vector; its length is taken and its elements indexed.}
\item{e}{Passed to \code{.ghc_unif}.}
}
\description{
A step of the hindsr_native implementation. Called by \code{hindsr}. See the file header for the source the module follows.
}
\value{
The value of \code{lapply}.
}
\keyword{internal}
