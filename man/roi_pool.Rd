% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{roi_pool}
\alias{roi_pool}
\title{roi_pool}
\usage{
roi_pool(features, box, out_size = 2L, stride = 1)
}
\arguments{
\item{features}{See Usage.}
\item{box}{See Usage.}
\item{out_size}{Defaults to \code{2L}.}
\item{stride}{Defaults to \code{1}.}
}
\description{
Part of the masrcn_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{pooled}, \code{quantised_box}, \code{quantisation_shift}, \code{caveat}.
}
\keyword{internal}
