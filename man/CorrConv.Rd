% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{CorrConv}
\alias{CorrConv}
\title{Correlation IS convolution with one sequence reversed}
\usage{
CorrConv(x, y)
}
\arguments{
\item{x}{See Usage.}
\item{y}{See Usage.}
}
\description{
A step of the rangayyan_ccf implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{ccf}, \code{via_convolution}, \code{lags}, \code{max_difference}, \code{identity_holds}, \code{correlation_is_convolution_with_one_reversed}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
CorrConv(V, V)
}
