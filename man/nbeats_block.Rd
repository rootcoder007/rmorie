% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{nbeats_block}
\alias{nbeats_block}
\title{nbeats_block}
\usage{
nbeats_block(window, horizon, kind = "generic", degree = 2, harmonics = 3, ridge = 1e-08)
}
\arguments{
\item{window}{See Usage.}
\item{horizon}{See Usage.}
\item{kind}{Defaults to \code{"generic"}.}
\item{degree}{Defaults to \code{2}.}
\item{harmonics}{Defaults to \code{3}.}
\item{ridge}{Defaults to \code{1e-08}.}
}
\description{
Part of the nbeats_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{backcast}, \code{forecast}, \code{theta}.
}
\keyword{internal}
