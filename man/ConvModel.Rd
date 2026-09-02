% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{ConvModel}
\alias{ConvModel}
\title{Eq (4.61): y(t) = x(t) * h(t), the model homomorphic DEconvolution}
\usage{
ConvModel(x, h) 
}
\arguments{
\item{x}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{h}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
addresses.
}
\value{
A list with \code{y}, \code{n}, \code{n_x}, \code{n_h}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
ConvModel(V, V)
}
