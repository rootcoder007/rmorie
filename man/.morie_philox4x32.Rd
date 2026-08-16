% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_philox4x32}
\alias{.morie_philox4x32}
\title{Counter: an (n x 4) numeric matrix of 32-bit words; key: length-2}
\usage{
.morie_philox4x32(counter, key, rounds = .MORIE_PHILOX_ROUNDS)
}
\arguments{
\item{counter}{See Usage.}
\item{key}{See Usage.}
\item{rounds}{Defaults to \code{.MORIE_PHILOX_ROUNDS}.}
}
\description{
numeric Part of the rng_native implementation; see the file header for the source it follows.
}
\value{
The value of \code{ctr}, as built in the body.
}
\keyword{internal}
