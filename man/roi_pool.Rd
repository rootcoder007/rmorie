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
\item{features}{Passed to \code{.masrcn_mat}.}
\item{box}{A vector; indexed elementwise.}
\item{out_size}{Defaults to \code{2L}.}
\item{stride}{Defaults to \code{1}.}
}
\description{
A step of the masrcn_native implementation. Called by \code{alignment_error}, \code{morie_masrcn}. See the file header for the source the module follows.
}
\value{
A list with \code{pooled}, \code{quantised_box}, \code{quantisation_shift}, \code{caveat}.
}
\keyword{internal}
