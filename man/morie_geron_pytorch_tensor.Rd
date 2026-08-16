% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_geron_pytorch_tensor}
\alias{morie_geron_pytorch_tensor}
\title{Torch-style tensor construction (numpy/R-backed, no torch call) (Geron Ch 10, hmpttn)}
\usage{
morie_geron_pytorch_tensor(x, device = "cpu", dtype = NULL)
}
\arguments{
\item{x}{Data.}
\item{device}{"cpu", "cuda" or "mps".}
\item{dtype}{Optional torch dtype name.}
}
\description{
Torch-style tensor construction (numpy/R-backed, no torch call) (Geron Ch 10, hmpttn)
}
\value{
A list with \code{tensor}, \code{dtype}, \code{numpy_dtype}, \code{device}, \code{shape}, \code{ndim}, \code{itemsize}, \code{nbytes}, \code{strides}, \code{dtype_changed}, \code{on_device}, \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_geron_pytorch_tensor(V)
}
