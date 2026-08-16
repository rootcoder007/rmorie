% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_k05_gtime}
\alias{.morie_k05_gtime}
\title{.morie_k05_gtime}
\usage{
.morie_k05_gtime(times, e_times, how)
}
\arguments{
\item{times}{A vector; its length is taken and its elements indexed.}
\item{e_times}{See Usage.}
\item{how}{One of \code{"identity"}, \code{"log"}, \code{"rank"}.}
}
\description{
A step of the k05_tranche2 implementation. Called by \code{morie_scaled_schoenfeld}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
