% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{narm_attention_weights}
\alias{narm_attention_weights}
\title{narm_attention_weights}
\usage{
narm_attention_weights(h_t, H, A1, A2, v) 
}
\arguments{
\item{h_t}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{H}{A matrix; passed to \code{as.matrix}.}
\item{A1}{A matrix; passed to \code{\%*\%}.}
\item{A2}{A matrix; passed to \code{\%*\%}.}
\item{v}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the narm_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{narm_softmax}.
}
\keyword{internal}
\examples{
narm_attention_weights(h_t = 5L, H = 0.5, A1 = c(1, 2, 3, 4, 5, 6, 7, 8),
  A2 = c(1, 2, 3, 4, 5, 6, 7, 8), v = c(1, 2, 3, 4, 5, 6, 7, 8))
}
