% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{MultModel}
\alias{MultModel}
\title{Eq (4.58): y(t) = x(t) p(t), the model a multiplicative homomorphic}
\usage{
MultModel(x, p) 
}
\arguments{
\item{x}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{p}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
system addresses.
}
\value{
A list with \code{y}, \code{x}, \code{p}, \code{n}, \code{separable_by_log}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
MultModel(V, V)
}
