% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_geron_padding}
\alias{morie_geron_padding}
\title{Zero-padding for valid/same convolutions (Geron Ch 12, hmpd)}
\usage{
morie_geron_padding(x, pad_h = NULL, pad_w = NULL, kernel_size = NULL, stride = 1) 
}
\arguments{
\item{x}{Array (h, w), (h, w, c) or (n, h, w, c).}
\item{pad_h,pad_w}{Explicit padding.}
\item{kernel_size}{Compute same-padding from a kernel instead.}
\item{stride}{Output-size bookkeeping.}
}
\description{
Zero-padding for valid/same convolutions (Geron Ch 12, hmpd)
}
\value{
A list with \code{padded}, \code{pad_h}, \code{pad_w}, \code{output_shape}, \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
morie_geron_padding(M)
}
