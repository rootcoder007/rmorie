% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{roi_align}
\alias{roi_align}
\title{roi_align}
\usage{
roi_align(features, box, out_size = 2L, stride = 1, samples = 2L)
}
\arguments{
\item{features}{See Usage.}
\item{box}{See Usage.}
\item{out_size}{Defaults to \code{2L}.}
\item{stride}{Defaults to \code{1}.}
\item{samples}{Defaults to \code{2L}.}
}
\description{
Part of the masrcn_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{pooled}, \code{exact_box}, \code{samples_per_bin}, \code{note}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
roi_align(V, V)
}
