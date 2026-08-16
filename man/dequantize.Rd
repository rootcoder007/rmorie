% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{dequantize}
\alias{dequantize}
\title{dequantize}
\usage{
dequantize(tokens, bins)
}
\arguments{
\item{tokens}{See Usage.}
\item{bins}{A list; the body reads \code{$centers} from it.}
}
\description{
A step of the chronos_native implementation. Called by \code{detokenize}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
b <- uniform_bins(-3, 3, n_bins = 16L)
dequantize(quantize(c(-2.5, 0, 1.7), b)$tokens, b)
}
