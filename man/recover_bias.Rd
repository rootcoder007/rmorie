% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{recover_bias}
\alias{recover_bias}
\title{recover_bias}
\usage{
recover_bias(alpha, y, grad, C) 
}
\arguments{
\item{alpha}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{y}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{grad}{A vector; indexed elementwise.}
\item{C}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the svmopt_native implementation. Called by \code{smo}. See the file header for the source the module follows.
}
\value{
A list with \code{b}, \code{n_free}, \code{bracketed}, \code{note}.
}
\keyword{internal}
\examples{
recover_bias(alpha = 0.5, y = c(1, 2, 3, 4, 5, 6, 7, 8), grad = c(1, 2, 3, 4, 5, 6, 7, 8), C = c(1, 2, 3, 4, 5, 6, 7, 8))
}
