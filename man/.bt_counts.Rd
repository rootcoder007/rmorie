% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.bt_counts}
\alias{.bt_counts}
\title{Resample count vectors: one row per bootstrap replication}
\usage{
.bt_counts(n, B, rng, exhaustive)
}
\arguments{
\item{n}{A count; the body uses it as \code{seq_len(...)}.}
\item{B}{A count; the body uses it as \code{seq_len(...)}.}
\item{rng}{Numeric; combined arithmetically in the body.}
\item{exhaustive}{A flag; the body branches on it.}
}
\description{
A step of the btmult implementation. Called by \code{Btcimed}, \code{Btmult}, \code{Btvarm}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
