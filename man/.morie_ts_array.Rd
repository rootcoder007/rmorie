% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_ts_array}
\alias{.morie_ts_array}
\title{Boot:::ts.array -- block start/length arrays for the block bootstrap}
\usage{
.morie_ts_array(n, n.sim, R, l, sim, endcorr)
}
\arguments{
\item{n}{Numeric; combined arithmetically in the body.}
\item{n.sim}{Numeric; combined arithmetically in the body.}
\item{R}{A count; the body uses it as \code{rep(...)}.}
\item{l}{A count; the body uses it as \code{rep(...)}.}
\item{sim}{Compared against \code{"geom"}.}
\item{endcorr}{A flag; the body branches on it.}
}
\description{
A step of the boot_native implementation. Called by \code{morie_tsboot}. See the file header for the source the module follows.
}
\value{
A list with \code{starts}, \code{lengths}.
}
\keyword{internal}
