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
\item{window}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{horizon}{Coerced to integer by the body, with \code{as.integer}.}
\item{blocks}{See Usage.}
\item{ridge}{Passed to \code{nbeats_block}. Defaults to \code{1e-08}.}
}
\description{
A step of the nbeats_native implementation. Called by \code{.ngnest_nbeats_stack}, \code{nbeats_forecast}. See the file header for the source the module follows.
}
\value{
A list with \code{forecast}, \code{residual}, \code{trace}.
}
\keyword{internal}
