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
\item{features}{Passed to \code{.masrcn_mat}.}
\item{box}{A vector; indexed elementwise.}
\item{out_size}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{2L}.}
\item{stride}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1}.}
\item{samples}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{2L}.}
}
\description{
A step of the masrcn_native implementation. Called by \code{morie_masrcn}. See the file header for the source the module follows.
}
\value{
A list with \code{pooled}, \code{exact_box}, \code{samples_per_bin}, \code{note}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
roi_align(V, V)
}
