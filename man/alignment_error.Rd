% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{alignment_error}
\alias{alignment_error}
\title{alignment_error}
\usage{
alignment_error(features, box, out_size = 2L, stride = 1)
}
\arguments{
\item{features}{See Usage.}
\item{box}{See Usage.}
\item{out_size}{Defaults to \code{2L}.}
\item{stride}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1}.}
}
\description{
A step of the masrcn_native implementation. Called by \code{morie_masrcn}. See the file header for the source the module follows.
}
\value{
A list with \code{feature_shift}, \code{input_pixel_shift}, \code{stride}, \code{note}.
}
\keyword{internal}
