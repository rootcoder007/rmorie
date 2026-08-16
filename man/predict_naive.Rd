% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{predict_naive}
\alias{predict_naive}
\title{Eq. (1) as written -- the O(kn^2) double sum -------------------}
\usage{
predict_naive(x, w0, w, V)
}
\arguments{
\item{x}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{w0}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{w}{Numeric; combined arithmetically in the body.}
\item{V}{A vector; indexed elementwise.}
}
\description{
A step of the fmFM_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{s}, as built in the body.
}
\keyword{internal}
\examples{
predict_naive(x = c(1, 2, 3, 4, 5, 6, 7, 8), w0 = c(1, 2, 3, 4, 5, 6, 7, 8), w = c(1, 2, 3, 4, 5, 6, 7, 8), V = c(1, 2, 3, 4, 5, 6, 7, 8))
}
