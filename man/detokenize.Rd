% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{detokenize}
\alias{detokenize}
\title{detokenize}
\usage{
detokenize(tokens, bins, scale)
}
\arguments{
\item{tokens}{Passed to \code{dequantize}.}
\item{bins}{Passed to \code{dequantize}.}
\item{scale}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the chronos_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
set.seed(2)
b <- uniform_bins(-3, 3, n_bins = 16L)
x <- rnorm(10)
tk <- tokenize(x, b, add_eos = FALSE)
detokenize(tk$tokens, b, tk$scale)
}
