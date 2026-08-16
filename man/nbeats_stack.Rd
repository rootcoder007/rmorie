% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{nbeats_stack}
\alias{nbeats_stack}
\title{nbeats_stack}
\usage{
nbeats_stack(window, horizon, blocks, ridge = 1e-08)
}
\arguments{
\item{window}{See Usage.}
\item{horizon}{See Usage.}
\item{blocks}{See Usage.}
\item{ridge}{Defaults to \code{1e-08}.}
}
\description{
Part of the nbeats_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{forecast}, \code{residual}, \code{trace}.
}
\keyword{internal}
